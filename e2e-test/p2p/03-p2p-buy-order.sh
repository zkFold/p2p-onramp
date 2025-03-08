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

barbaraRedeemer=$assets/barbaraSoldRedeemer.cbor
barbaraOut=$(cardano-cli conway transaction txid --tx-file "$keypath/barbaraSells.tx")#0
barbaraOutResolved=$(cardano-cli conway query utxo --address $(cat $keypath/onRamp.addr) --testnet-magic $mN --out-file /dev/stdout |
                         jq -r --arg key "$barbaraOut" '.[$key]')
barbaraLovelace=$(cardano-cli conway query utxo --address $(cat $keypath/onRamp.addr) --testnet-magic $mN --out-file /dev/stdout |
                          jq -r --arg key "$barbaraOut" '.[$key].value.lovelace')

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

random_integer () {
    local min=$1
    local max=$2
    echo $(( RANDOM % (max - min + 1) + min ))
}

#-------------------------------- :buy order: ----------------------------------

echo ""
echo "Generating buyer's datum and redeemer..."

cabal run p2p-buy-order -- "charlie" "$charlieAddr" "barbara" "$barbaraOutResolved"

exit 1  # ToDo: 'verifyEd25519Signature' needs PubKey, not PubKeyHash.

echo ""
echo "Buy-order transaction..."

in1=$(cardano-cli conway query utxo --address $(cat $keypath/barbara.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r 'keys[0]')
collateral=$in1

current_time=$(date +%s)
current_slot=$(posix_to_slot $current_time)

slot1=$(($current_slot - 5))
slot2=$(($current_slot + 25))

cardano-cli conway transaction build \
    --testnet-magic $mN \
    --tx-in $in1 \
    --tx-in $barbaraOut \
    --tx-in-script-file $onRampPlutus \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-cbor-file $barbaraRedeemer \
    --tx-in-collateral $collateral \
    --tx-out "$onRampAddr + $barbaraLovelace lovelace" \
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
