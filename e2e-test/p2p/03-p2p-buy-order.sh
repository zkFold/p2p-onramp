#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

previewmagic=2
assets=../assets
keypath=./p2p/keys
privpath=./p2p/priv

mN=$(cat $privpath/testnet.flag)

# Wait time (in seconds) before querying blockchain
if [ $mN == $previewmagic ]; then
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
    echo $(( inv_slot_length * (posix_time - system_start) ))
}

to_plutus_posix () {
    local posix_time=$1
    local plutus_posix_time=$(( 1000 * posix_time ))
    echo $plutus_posix_time
}

utxo_resolved () {
    local oref=$1
    echo $(cardano-cli conway query utxo --address $onRampAddr --testnet-magic $mN --out-file /dev/stdout |
               jq -c --arg key "$oref" '.[$key]')
}

is_selected_seller () {
    local selected_oref=$1
    local seller_name=$2
    local seller_oref=$(cardano-cli conway transaction txid --tx-file "$keypath/${seller_name}Sells.tx")#0

    if [ "$seller_oref" == "$selected_oref" ]; then
        return 0  # true
    else
        return 1  # false
    fi
}

read -r -a sellerNamesAll < $keypath/sellerNamesAll.txt

identify_selected_seller_name () {
    local selected_oref=$1

    for name in "${sellerNamesAll[@]}"; do
        if is_selected_seller "$selected_oref" "$name"; then
	    printf "$name" > $assets/sellChoiceName.txt
            return 0
        fi
    done
    echo "Unable to identify selected seller."
    exit 1
}

#--------------------------- :select best seller: ------------------------------

echo ""
echo "Choosing best sell offer..."
echo ""

if [ ! -f "$assets/sellChoiceOref.txt" ]; then
    onRampUtxos=$(cardano-cli conway query utxo --address $onRampAddr --testnet-magic $mN --out-file /dev/stdout | jq -c 'to_entries')

    cabal run p2p-choose-offer -- "$onRampUtxos"  # Selects TxOutRef for best sell offer
fi

sellerOut=$(cat $assets/sellChoiceOref.txt)

identify_selected_seller_name "$sellerOut"        # Identify name of selected seller
sellerName=$(cat $assets/sellChoiceName.txt)
sellerNameCap=$(echo $sellerName | sed -E 's/^(.)/\U\1/')

echo "Selected seller: $sellerNameCap"

sellerRedeemer="$assets/${sellerName}SoldRedeemer.cbor"
sellerOutResolved=$(utxo_resolved $sellerOut)
sellerLovelace=$(echo $sellerOutResolved | jq '.value.lovelace')

#-------------------------------- :buy order: ----------------------------------

echo ""
echo "Generating buyer's datum and redeemer..."

now=$(date +%s)
deadline=$(to_plutus_posix $((now + 300)))  # adding five minutes to current time

cabal run p2p-buy-order -- "charlie" $charlieAddr $sellerName $sellerOutResolved $deadline

echo ""
echo "Buy-order transaction..."

in1=$(cardano-cli conway query utxo --address $(cat $keypath/charlie.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r 'keys[0]')
collateral=$in1

now=$(date +%s)
slot0=$(posix_to_slot $now)
slot2=$(($slot0 + 60))

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
