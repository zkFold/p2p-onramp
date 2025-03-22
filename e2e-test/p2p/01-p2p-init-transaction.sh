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

mkdir -p $assets

# Wait time (in seconds) before querying blockchain
if [ $mN == $previewmagic ]; then
    pause=7
else
    pause=4
fi

in1=$(cardano-cli conway query utxo --address $(cat $keypath/alice.addr) --testnet-magic $mN --out-file /dev/stdout |
	  jq -r 'to_entries | map(select(.value.value.lovelace > 5000000)) | .[0].key')

echo ""
echo "Initialization..."

#--------------------------------- :create onRamp script: --------------------------------

echo ""
echo "Creating 'onRamp' script..."

aliceAddress=$(cat $keypath/alice.addr)

cabal run p2p-init-transaction -- "alice" $aliceAddress

#------------------------------------- :onRamp setup: ------------------------------------

cardano-cli conway address build \
    --payment-script-file "$assets/onRamp.plutus" \
    --out-file "$keypath/onRamp.addr" \
    --testnet-magic $mN

#-------------------------------- :protocol parameters: ------------------------------

cardano-cli conway query protocol-parameters \
  --testnet-magic $mN \
  --out-file $assets/protocol.json

#---------------------------------- :initialize Barbara: ---------------------------------

echo ""
echo "Initializing sellers..."

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

echo ""
echo "Initializing buyer..."

cardano-cli conway address key-gen \
  --verification-key-file $keypath/charlie.vkey \
  --signing-key-file $keypath/charlie.skey

cardano-cli conway address build \
  --payment-verification-key-file $keypath/charlie.vkey \
  --out-file $keypath/charlie.addr \
  --testnet-magic $mN

charlieAddress=$(cat $keypath/charlie.addr)

#---------------------------- :fund other wallets: -----------------------------

echo ""
echo "Funding wallets..."

funds1=50000000  # 50 ADA
funds2=10000000  # 10 ADA

cardano-cli conway transaction build \
    --testnet-magic $mN \
    --tx-in $in1 \
    --tx-out "$barbaraAddress + $funds1" \
    --tx-out "$bobAddress + $funds1" \
    --tx-out "$brandonAddress + $funds1" \
    --tx-out "$charlieAddress + $funds2" \
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

#-------------------------------- :epilogue: -------------------------------

echo ""
echo "Initialization completed."
echo ""
