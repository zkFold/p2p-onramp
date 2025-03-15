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
cancelRedeemer=$assets/cancel.cbor

#---------------------------------- :macros: -----------------------------------

current_time=$(date +%s)
current_slot=$(cardano-cli conway query tip --testnet-magic $mN | jq -r '.slot')
system_start=$((current_time - current_slot/inv_slot_length))

posix_to_slot () {
    local posix_time=$1
    echo $(( 10 * ($posix_time - $system_start) ))
}

#------------------------------- :cancel Barbara: ------------------------------

echo "Cancel sell orders..."

barbaraOut=$(cardano-cli conway transaction txid --tx-file "$keypath/barbaraSells.tx")#0
barbaraAddr=$(cat $keypath/barbara.addr)
isBarbaraUnspent=$(cardano-cli conway query utxo --address $(cat $keypath/onRamp.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$barbaraOut" 'has($key) | tostring')

if [ $isBarbaraUnspent == "false" ]; then
    echo ""
    echo "Barbara has sold or closed sell offer; nothing to cancel."
else
    echo ""
    echo "Canceling Barbara's order..."

    in1=$(cardano-cli conway query utxo --address $(cat $keypath/barbara.addr) --testnet-magic $mN --out-file /dev/stdout |
	      jq -r 'keys[0]')
    collateral=$in1
    barbaraLovelace=$(cardano-cli conway query utxo --address $(cat $keypath/onRamp.addr) --testnet-magic $mN --out-file /dev/stdout |
			  jq -r --arg key "$barbaraOut" '.[$key].value.lovelace')

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
	--tx-in-redeemer-cbor-file $cancelRedeemer \
	--tx-in-collateral $collateral \
	--tx-out "$barbaraAddr + $barbaraLovelace lovelace" \
	--change-address $(cat $keypath/barbara.addr) \
	--invalid-before $slot1 \
	--invalid-hereafter $slot2 \
	--out-file $keypath/barbaraCancels.txbody

    cardano-cli conway transaction sign \
	--testnet-magic $mN \
	--tx-body-file $keypath/barbaraCancels.txbody \
	--signing-key-file $keypath/barbara.skey \
	--out-file $keypath/barbaraCancels.tx

    cardano-cli conway transaction submit \
	--testnet-magic $mN \
	--tx-file $keypath/barbaraCancels.tx

    echo "Barbara's order has ben canceled."
fi

#--------------------------------- :cancel Bob: --------------------------------

bobOut=$(cardano-cli conway transaction txid --tx-file "$keypath/bobSells.tx")#0
bobAddr=$(cat $keypath/bob.addr)
isBobUnspent=$(cardano-cli conway query utxo --address $(cat $keypath/onRamp.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$bobOut" 'has($key) | tostring')

if [ $isBobUnspent == "false" ]; then
    echo ""
    echo "Bob has sold or closed sell offer; nothing to cancel."
else
    echo ""
    echo "Canceling Bob's order..."

    in1=$(cardano-cli conway query utxo --address $(cat $keypath/bob.addr) --testnet-magic $mN --out-file /dev/stdout |
	      jq -r 'keys[0]')
    collateral=$in1
    bobLovelace=$(cardano-cli conway query utxo --address $(cat $keypath/onRamp.addr) --testnet-magic $mN --out-file /dev/stdout |
			  jq -r --arg key "$bobOut" '.[$key].value.lovelace')

    current_time=$(date +%s)
    current_slot=$(posix_to_slot $current_time)

    slot1=$(($current_slot - 5))
    slot2=$(($current_slot + 25))

    cardano-cli conway transaction build \
        --testnet-magic $mN \
	--tx-in $in1 \
	--tx-in $bobOut \
	--tx-in-script-file $onRampPlutus \
	--tx-in-inline-datum-present \
	--tx-in-redeemer-cbor-file $cancelRedeemer \
	--tx-in-collateral $collateral \
	--tx-out "$bobAddr + $bobLovelace lovelace" \
	--change-address $(cat $keypath/bob.addr) \
	--invalid-before $slot1 \
	--invalid-hereafter $slot2 \
	--out-file $keypath/bobCancels.txbody

    cardano-cli conway transaction sign \
	--testnet-magic $mN \
	--tx-body-file $keypath/bobCancels.txbody \
	--signing-key-file $keypath/bob.skey \
	--out-file $keypath/bobCancels.tx

    cardano-cli conway transaction submit \
	--testnet-magic $mN \
	--tx-file $keypath/bobCancels.tx

    echo "Bob's order has been canceled."
fi

#------------------------------- :cancel Brandon: ------------------------------

brandonOut=$(cardano-cli conway transaction txid --tx-file "$keypath/brandonSells.tx")#0
brandonAddr=$(cat $keypath/brandon.addr)
isBrandonUnspent=$(cardano-cli conway query utxo --address $(cat $keypath/onRamp.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$brandonOut" 'has($key) | tostring')

if [ $isBrandonUnspent == "false" ]; then
    echo ""
    echo "Brandon has sold or closed sell offer; nothing to cancel."
else
    echo ""
    echo "Canceling Brandon's order..."

    in1=$(cardano-cli conway query utxo --address $(cat $keypath/brandon.addr) --testnet-magic $mN --out-file /dev/stdout |
	      jq -r 'keys[0]')
    collateral=$in1
    brandonLovelace=$(cardano-cli conway query utxo --address $(cat $keypath/onRamp.addr) --testnet-magic $mN --out-file /dev/stdout |
			  jq -r --arg key "$brandonOut" '.[$key].value.lovelace')

    current_time=$(date +%s)
    current_slot=$(posix_to_slot $current_time)

    slot1=$(($current_slot - 5))
    slot2=$(($current_slot + 25))

    cardano-cli conway transaction build \
        --testnet-magic $mN \
	--tx-in $in1 \
	--tx-in $brandonOut \
	--tx-in-script-file $onRampPlutus \
	--tx-in-inline-datum-present \
	--tx-in-redeemer-cbor-file $cancelRedeemer \
	--tx-in-collateral $collateral \
	--tx-out "$brandonAddr + $brandonLovelace lovelace" \
	--change-address $(cat $keypath/brandon.addr) \
	--invalid-before $slot1 \
	--invalid-hereafter $slot2 \
	--out-file $keypath/brandonCancels.txbody

    cardano-cli conway transaction sign \
	--testnet-magic $mN \
	--tx-body-file $keypath/brandonCancels.txbody \
	--signing-key-file $keypath/brandon.skey \
	--out-file $keypath/brandonCancels.tx

    cardano-cli conway transaction submit \
	--testnet-magic $mN \
	--tx-file $keypath/brandonCancels.tx

    echo "Brandon's order has been canceled."
fi
