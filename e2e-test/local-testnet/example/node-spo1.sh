#!/usr/bin/env bash

CARDANO_NODE="${CARDANO_NODE:-cardano-node}"

$CARDANO_NODE run \
  --config                          'local-testnet/example/configuration.yaml' \
  --topology                        'local-testnet/example/node-spo1/topology.json' \
  --database-path                   'local-testnet/example/node-spo1/db' \
  --socket-path                     'local-testnet/example/node-spo1/node.sock' \
  --shelley-kes-key                 'local-testnet/example/node-spo1/kes.skey' \
  --shelley-vrf-key                 'local-testnet/example/node-spo1/vrf.skey' \
  --byron-delegation-certificate    'local-testnet/example/node-spo1/byron-delegation.cert' \
  --byron-signing-key               'local-testnet/example/node-spo1/byron-delegate.key' \
  --shelley-operational-certificate 'local-testnet/example/node-spo1/opcert.cert' \
  --port                            3001 \
  | tee -a 'local-testnet/example/node-spo1/node.log'
