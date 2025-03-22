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
cancelRedeemer=$assets/cancel.cbor

#---------------------------------- :macros: -----------------------------------

current_time=$(date +%s)
current_slot=$(cardano-cli conway query tip --testnet-magic $mN | jq -r '.slot')
system_start=$((current_time - current_slot/inv_slot_length))

posix_to_slot () {
    local posix_time=$1
    echo $(( inv_slot_length * (posix_time - system_start) ))
}

cancel_sell_order () {
    local seller_name=$1
    local seller_name_cap=$(echo "$seller_name" | sed -E 's/^(.)/\U\1/')
    local seller_out=$(cardano-cli conway transaction txid --tx-file "$keypath/${seller_name}Sells.tx")#0
    local seller_addr=$(cat $keypath/${seller_name}.addr)
    local is_seller_unspent=$(cardano-cli conway query utxo --address $(cat $keypath/onRamp.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$seller_out" 'has($key) | tostring')

    if [ $is_seller_unspent == "false" ]; then
	echo ""
	echo "$seller_name_cap has sold or closed sell offer; nothing to cancel."
    else
	echo ""
	echo "Canceling ${seller_name_cap}'s order..."

	local in1=$(cardano-cli conway query utxo --address $seller_addr --testnet-magic $mN --out-file /dev/stdout |
			jq -r 'keys[0]')
	local collateral=$in1
	local sellerLovelace=$(cardano-cli conway query utxo --address $(cat $keypath/onRamp.addr) --testnet-magic $mN --out-file /dev/stdout |
				   jq -r --arg key "$seller_out" '.[$key].value.lovelace')

	local now=$(date +%s)
	local slot0=$(posix_to_slot $now)
	local slot1=$(($slot0 - 30))

	cardano-cli conway transaction build \
	    --testnet-magic $mN \
	    --tx-in $in1 \
	    --tx-in $seller_out \
	    --tx-in-script-file $onRampPlutus \
	    --tx-in-inline-datum-present \
	    --tx-in-redeemer-cbor-file $cancelRedeemer \
	    --tx-in-collateral $collateral \
	    --tx-out "$seller_addr + $sellerLovelace lovelace" \
	    --change-address $seller_addr \
	    --invalid-before $slot1 \
	    --out-file $keypath/${seller_name}Cancels.txbody

	cardano-cli conway transaction sign \
	    --testnet-magic $mN \
	    --tx-body-file $keypath/${seller_name}Cancels.txbody \
	    --signing-key-file $keypath/${seller_name}.skey \
	    --out-file $keypath/${seller_name}Cancels.tx

	cardano-cli conway transaction submit \
	    --testnet-magic $mN \
	    --tx-file $keypath/${seller_name}Cancels.tx

	echo "${seller_name_cap}'s order has ben canceled."
    fi
}

#---------------------------  :cancel sell orders: -----------------------------

echo "Cancel sell orders..."

cancel_sell_order "barbara"
cancel_sell_order "bob"
cancel_sell_order "brandon"

#--------------------------------- :epilogue: ----------------------------------

echo ""

while true; do
    isCanceled=$(cardano-cli conway query utxo --address $onRampAddr --testnet-magic $mN --out-file /dev/stdout | jq -c '. == {}')
    if [ $isCanceled == "false" ]; then
	echo "Waiting to see all sell orders canceled onchain..."
	sleep $pause
    else
	echo ""
	echo "Done."
	echo ""
	break
    fi
done
