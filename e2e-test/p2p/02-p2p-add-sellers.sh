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

onRamp_addr=$(cat $keypath/onRamp.addr)
seller_names_all=""

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

add_seller () {
    local seller_name=$1
    local seller_name_cap=$(echo "$seller_name" | sed -E 's/^(.)/\U\1/')

    local sell_price=$(random_integer 30 40)
    local lovelace_sold=$(random_integer 30000000 40000000)
    local seller_addr=$(cat $keypath/${seller_name}.addr)
    local now=$(date +%s)
    local deadline=$((now + 60))  # adding one minute to current time

    echo ""
    echo "$seller_name_cap sells $lovelace_sold lovelace..."
    echo ""

    # Arguments for cabal executable 'p2p-add-seller':
    # 1) Seller name
    # 2) Sell price
    # 3) Value sold (lovelace)
    # 4) Buy deadline

    cabal run p2p-add-seller -- $seller_name $sell_price $lovelace_sold $deadline

    echo "Sending ${seller_name_cap}'s sell offer UTxO..."

    in1=$(cardano-cli conway query utxo --address \
	      $(cat $keypath/${seller_name}.addr) --testnet-magic $mN --out-file /dev/stdout |
	            jq -r 'keys[0]')

    cardano-cli conway transaction build \
	--testnet-magic $mN \
	--tx-in $in1 \
	--tx-out "$onRamp_addr + $lovelace_sold lovelace" \
	--tx-out-inline-datum-cbor-file $assets/${seller_name}SellDatum.cbor \
	--change-address $seller_addr \
	--out-file $keypath/${seller_name}Sells.txbody

    cardano-cli conway transaction sign \
	--testnet-magic $mN \
	--tx-body-file $keypath/${seller_name}Sells.txbody \
	--signing-key-file $keypath/${seller_name}.skey \
	--out-file $keypath/${seller_name}Sells.tx

    cardano-cli conway transaction submit \
	--testnet-magic $mN \
	--tx-file $keypath/${seller_name}Sells.tx

    seller_names_all+="$seller_name "
}

#-------------------------------- :add sellers: --------------------------------

echo "Adding sellers..."

add_seller "barbara"
add_seller "bob"
add_seller "brandon"

#--------------------------------- :epilogue: ----------------------------------

echo "${seller_names_all% }" > $keypath/sellerNamesAll.txt

echo ""
