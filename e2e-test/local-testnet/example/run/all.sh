#!/usr/bin/env bash

local-testnet/example/node-spo1.sh &
local-testnet/example/node-spo2.sh &
local-testnet/example/node-spo3.sh &

wait
