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

unitDatum=$assets/unit.cbor
validityRedeemer=$assets/validityRedeemer.cbor
timedPlutus=$assets/timed.plutus

current_time=$(date +%s)
current_slot=$(cardano-cli conway query tip --testnet-magic $mN | jq -r '.slot')
system_start=$((current_time - current_slot/inv_slot_length))

posix_to_slot () {
    local posix_time=$1
    echo $(( 10 * ($posix_time - $system_start) ))
}

#------------------------------- :consume timed: -------------------------------

echo "Consuming 'timed'..."
echo ""

mark_time=$(($current_time - 60))

cabal run p2p-consume-transaction -- $mark_time

in1=$(cardano-cli conway query utxo --address $(cat $keypath/charlie.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r 'keys[0]')
collateral=$in1

in2=$(cardano-cli conway query utxo --address $(cat $keypath/timed.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r 'keys[0]')

echo "in1: $in1"
echo "in2: $in2"

current_time=$(date +%s)
current_slot=$(posix_to_slot $current_time)

slot1=$(($current_slot - 5))
slot2=$(($current_slot + 25))

echo "slot1: $slot1"
echo "slot2: $slot2"

cardano-cli conway transaction build \
    --testnet-magic $mN \
    --tx-in $in1 \
    --tx-in $in2 \
    --tx-in-script-file $timedPlutus \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-cbor-file $validityRedeemer \
    --tx-in-collateral $collateral \
    --tx-out "$(cat $keypath/charlie.addr) + 13000000" \
    --change-address $(cat $keypath/charlie.addr) \
    --invalid-before $slot1 \
    --invalid-hereafter $slot2 \
    --out-file $keypath/consumed.txbody

cardano-cli conway transaction sign \
    --testnet-magic $mN \
    --tx-body-file $keypath/consumed.txbody \
    --signing-key-file $keypath/charlie.skey \
    --out-file $keypath/consumed.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/consumed.tx

