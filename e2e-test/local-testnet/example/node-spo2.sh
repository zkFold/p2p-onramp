#!/usr/bin/env bash

CARDANO_NODE="${CARDANO_NODE:-cardano-node}"

$CARDANO_NODE run \
  --config                          'local-testnet/example/configuration.yaml' \
  --topology                        'local-testnet/example/node-spo2/topology.json' \
  --database-path                   'local-testnet/example/node-spo2/db' \
  --socket-path                     'local-testnet/example/node-spo2/node.sock' \
  --shelley-kes-key                 'local-testnet/example/node-spo2/kes.skey' \
  --shelley-vrf-key                 'local-testnet/example/node-spo2/vrf.skey' \
  --byron-delegation-certificate    'local-testnet/example/node-spo2/byron-delegation.cert' \
  --byron-signing-key               'local-testnet/example/node-spo2/byron-delegate.key' \
  --shelley-operational-certificate 'local-testnet/example/node-spo2/opcert.cert' \
  --port                            3002 \
  | tee -a 'local-testnet/example/node-spo2/node.log'
