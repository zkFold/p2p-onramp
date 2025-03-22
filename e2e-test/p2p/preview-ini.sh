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

if [ -d "./p2p" ]; then
    mkdir -p $assets
    mkdir -p $keypath
    mkdir -p $privpath
else
    echo "Please run script from directory 'e2e-test'."
    exit 1
fi

printf "$previewmagic" > $privpath/testnet.flag

./p2p/init-alice.sh
