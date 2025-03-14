#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

sanchomagic=4
assets=../assets
keypath=./p2p/keys
privpath=./p2p/priv

mN=$(cat $privpath/testnet.flag)

# Wait time (in seconds) before querying blockchain
if [ $mN == $sanchomagic ]; then
    pause=7
    inv_slot_length=1    
else
    pause=4
    inv_slot_length=10
fi

onRampAddr=$(cat $keypath/onRamp.addr)
onRampPlutus=$assets/onRamp.plutus

charlieDatum=$assets/charlieBoughtDatum.cbor
charlieAddr=$(cat $keypath/charlie.addr)

#---------------------------------- :macros: -----------------------------------

current_time=$(date +%s)
current_slot=$(cardano-cli conway query tip --testnet-magic $mN | jq -r '.slot')
system_start=$((current_time - current_slot/inv_slot_length))

posix_to_slot () {
    local posix_time=$1
    echo $(( 10 * ($posix_time - $system_start) ))
}

seller_utxo () {
    local seller_name=$1
    local tx_out_ref=$(cardano-cli conway transaction txid --tx-file "$keypath/${seller_name}Sells.tx")#0
    echo $(cardano-cli conway query utxo --address $onRampAddr --testnet-magic $mN --out-file /dev/stdout |
               jq -c --arg key "$tx_out_ref" '.[$key]')
}

#--------------------------- :select best seller: ------------------------------

echo ""
echo "Choosing best sell offer..."
echo ""

# Example: selecting best offer among three sellers.
cabal run p2p-choose-offer  -- "barbara" $(seller_utxo "barbara") \
                               "bob" $(seller_utxo "bob") \
                               "brandon" $(seller_utxo "brandon")

seller=$(cat $assets/sellerChoice.txt)
echo "Selected sell offer: $seller"

sellerRedeemer="$assets/${seller}SoldRedeemer.cbor"
sellerOut=$(cardano-cli conway transaction txid --tx-file "$keypath/${seller}Sells.tx")#0
sellerOutResolved=$(cardano-cli conway query utxo --address $(cat $keypath/onRamp.addr) --testnet-magic $mN --out-file /dev/stdout |
                         jq -r --arg key "$sellerOut" '.[$key]')
sellerLovelace=$(cardano-cli conway query utxo --address $(cat $keypath/onRamp.addr) --testnet-magic $mN --out-file /dev/stdout |
                          jq -r --arg key "$sellerOut" '.[$key].value.lovelace')

#-------------------------------- :buy order: ----------------------------------

echo ""
echo "Generating buyer's datum and redeemer..."

cabal run p2p-buy-order -- "charlie" $charlieAddr $seller $(seller_utxo $seller)

echo ""
echo "Buy-order transaction..."
echo ""

in1=$(cardano-cli conway query utxo --address $(cat $keypath/charlie.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r 'keys[0]')
collateral=$in1

current_time=$(date +%s)
current_slot=$(posix_to_slot $current_time)

slot1=$(($current_slot - 5))
slot2=$(($current_slot + 25))

cardano-cli conway transaction build \
    --testnet-magic $mN \
    --tx-in $in1 \
    --tx-in $sellerOut \
    --tx-in-script-file $onRampPlutus \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-cbor-file $sellerRedeemer \
    --tx-in-collateral $collateral \
    --tx-out "$onRampAddr + $sellerLovelace lovelace" \
    --tx-out-inline-datum-cbor-file $charlieDatum \
    --change-address $(cat $keypath/charlie.addr) \
    --invalid-before $slot1 \
    --invalid-hereafter $slot2 \
    --out-file $keypath/charlieBuys.txbody

    cardano-cli conway transaction sign \
	--testnet-magic $mN \
	--tx-body-file $keypath/charlieBuys.txbody \
	--signing-key-file $keypath/charlie.skey \
	--out-file $keypath/charlieBuys.tx

    cardano-cli conway transaction submit \
	--testnet-magic $mN \
	--tx-file $keypath/charlieBuys.tx

buysTx=$(cardano-cli conway transaction txid --tx-file "$keypath/charlieBuys.tx")
buysOut=$buysTx#0
while true; do
    txOnChain=$(cardano-cli conway query utxo --address $onRampAddr --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$buysOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see buy order tx onchain..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $buysTx"
	echo ""
	break
    fi
done
