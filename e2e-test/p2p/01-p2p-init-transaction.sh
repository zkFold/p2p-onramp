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

mkdir -p $assets

# Wait time (in seconds) before querying blockchain
if [ $mN == $sanchomagic ]; then
    pause=7
else
    pause=4
fi

unitDatum=$assets/unit.cbor

in1=$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file /dev/stdout |
	  jq -r 'to_entries | map(select(.value.value.lovelace > 5000000)) | .[0].key')

echo ""
echo "Initialization..."

#-------------------------------- :protocol parameters: ------------------------------

cardano-cli conway query protocol-parameters \
  --testnet-magic $mN \
  --out-file $assets/protocol.json

#---------------------------------- :initialize Barbara: ---------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/barbara.vkey \
  --signing-key-file $keypath/barbara.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/barbara.vkey \
  --out-file $keypath/barbara.addr \
  --testnet-magic $mN

barbaraAddress=$(cat $keypath/barbara.addr)

#---------------------------------- :initialize Bob: ---------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/bob.vkey \
  --signing-key-file $keypath/bob.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/bob.vkey \
  --out-file $keypath/bob.addr \
  --testnet-magic $mN

bobAddress=$(cat $keypath/bob.addr)

#---------------------------------- :initialize Brandon: ---------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/brandon.vkey \
  --signing-key-file $keypath/brandon.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/brandon.vkey \
  --out-file $keypath/brandon.addr \
  --testnet-magic $mN

brandonAddress=$(cat $keypath/brandon.addr)

#---------------------------------- :initialize Charlie: ---------------------------------

cardano-cli conway address key-gen \
  --verification-key-file $keypath/charlie.vkey \
  --signing-key-file $keypath/charlie.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/charlie.vkey \
  --out-file $keypath/charlie.addr \
  --testnet-magic $mN

charlieAddress=$(cat $keypath/charlie.addr)

#------------------------------- :create scripts: ------------------------------

cabal run p2p-init-transaction

#-------------------------------- :timed setup: --------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/timed.plutus" \
    --out-file "$keypath/timed.addr" \
    --testnet-magic $mN

#---------------------------- :fund other wallets: -----------------------------

echo ""
echo "Funding Barbara, Bob, Brandon and Charlie..."
echo ""

cardano-cli conway transaction build \
    --testnet-magic $mN \
    --tx-in $in1 \
    --tx-out "$(cat $keypath/barbara.addr) + 100000000" \
    --tx-out "$(cat $keypath/bob.addr) + 100000000" \
    --tx-out "$(cat $keypath/brandon.addr) + 100000000" \
    --tx-out "$(cat $keypath/charlie.addr) + 5000000" \
    --change-address $(cat $keypath/alice.addr) \
    --out-file $keypath/funding.txbody

cardano-cli conway transaction sign \
    --testnet-magic $mN \
    --tx-body-file $keypath/funding.txbody \
    --signing-key-file $keypath/alice.skey \
    --out-file $keypath/funding.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/funding.tx

fundingTx=$(cardano-cli conway transaction txid --tx-file "$keypath/funding.tx")
fundingOut=$fundingTx#0
while true; do
    txOnChain=$(cardano-cli conway query utxo --address $(cat $keypath/barbara.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$fundingOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see initial funding tx onchain..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $fundingTx"
	break
    fi
done

#-------------------------------- :fund timed: --------------------------------

echo ""
echo "Funding timed script"
echo ""

cardano-cli conway transaction build \
    --testnet-magic $mN \
    --tx-in $fundingTx#4 \
    --tx-out "$(cat $keypath/timed.addr) + 13000000 lovelace" \
    --tx-out-inline-datum-cbor-file $unitDatum \
    --change-address $(cat $keypath/alice.addr) \
    --out-file $keypath/iniTimed.txbody

cardano-cli conway transaction sign \
    --testnet-magic $mN \
    --tx-body-file $keypath/iniTimed.txbody \
    --signing-key-file $keypath/alice.skey \
    --out-file $keypath/iniTimed.tx

cardano-cli conway transaction submit \
    --testnet-magic $mN \
    --tx-file $keypath/iniTimed.tx

iniTimedTx=$(cardano-cli conway transaction txid --tx-file "$keypath/iniTimed.tx")
iniTimedOut=$iniTimedTx#0
while true; do
    txOnChain=$(cardano-cli conway query utxo --address $(cat $keypath/timed.addr) --testnet-magic $mN --out-file /dev/stdout | jq -r --arg key "$iniTimedOut" 'has($key) | tostring')
    if [ $txOnChain == "false" ]; then
	echo "Waiting to see 'timed script' initial funding tx onchain..."
	sleep $pause
    else
	echo ""
	echo "Transaction Id: $iniTimedTx"
	break
    fi
done

#-------------------------------- :epilogue: -------------------------------

echo ""
echo "Initialization completed."
echo ""
