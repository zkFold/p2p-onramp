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
onRampAddr=$(cat $keypath/onRamp.addr)

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

#------------------------------- :Barbara sells: -------------------------------

echo "Adding sellers..."

sellPrice=$(random_integer 30 40)
lovelaceSold=$(random_integer 30000000 40000000)
barbaraAddr=$(cat $keypath/barbara.addr)
deadline=$(($current_time + 3))  # just adding 3 seconds to current time

echo ""
echo "Barbara sells $lovelaceSold lovelace..."
echo ""

cabal run p2p-add-seller -- "barbara" $sellPrice $lovelaceSold $barbaraAddr $deadline

in1=$(cardano-cli conway query utxo --address $(cat $keypath/barbara.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
    --testnet-magic $mN \
    --tx-in $in1 \
    --tx-out "$onRampAddr + $lovelaceSold lovelace" \
    --tx-out-inline-datum-cbor-file $assets/barbara.cbor \
    --change-address $barbaraAddr \
    --out-file $keypath/barbaraSells.txbody

cardano-cli conway transaction sign \
    --testnet-magic $mN \
    --tx-body-file $keypath/barbaraSells.txbody \
    --signing-key-file $keypath/barbara.skey \
    --out-file $keypath/barbaraSells.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/barbaraSells.tx

#--------------------------------- :Bob sells: ---------------------------------

sellPrice=$(random_integer 30 40)
lovelaceSold=$(random_integer 30000000 40000000)
bobAddr=$(cat $keypath/bob.addr)
deadline=$(($current_time + 3))  # just adding 3 seconds to current time

echo ""
echo "Bob sells $lovelaceSold lovelace..."
echo ""

cabal run p2p-add-seller -- "bob" $sellPrice $lovelaceSold $bobAddr $deadline

in2=$(cardano-cli conway query utxo --address $(cat $keypath/bob.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
    --testnet-magic $mN \
    --tx-in $in2 \
    --tx-out "$onRampAddr + $lovelaceSold lovelace" \
    --tx-out-inline-datum-cbor-file $assets/bob.cbor \
    --change-address $bobAddr \
    --out-file $keypath/bobSells.txbody

cardano-cli conway transaction sign \
    --testnet-magic $mN \
    --tx-body-file $keypath/bobSells.txbody \
    --signing-key-file $keypath/bob.skey \
    --out-file $keypath/bobSells.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/bobSells.tx

#------------------------------- :Brandon sells: -------------------------------

sellPrice=$(random_integer 30 40)
lovelaceSold=$(random_integer 30000000 40000000)
brandonAddr=$(cat $keypath/brandon.addr)
deadline=$(($current_time + 3))  # just adding 3 seconds to current time

echo ""
echo "Brandon sells $lovelaceSold lovelace..."
echo ""

cabal run p2p-add-seller -- "brandon" $sellPrice $lovelaceSold $brandonAddr $deadline

in3=$(cardano-cli conway query utxo --address $(cat $keypath/brandon.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r 'keys[0]')

cardano-cli conway transaction build \
    --testnet-magic $mN \
    --tx-in $in3 \
    --tx-out "$onRampAddr + $lovelaceSold lovelace" \
    --tx-out-inline-datum-cbor-file $assets/brandon.cbor \
    --change-address $brandonAddr \
    --out-file $keypath/brandonSells.txbody

cardano-cli conway transaction sign \
    --testnet-magic $mN \
    --tx-body-file $keypath/brandonSells.txbody \
    --signing-key-file $keypath/brandon.skey \
    --out-file $keypath/brandonSells.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/brandonSells.tx
