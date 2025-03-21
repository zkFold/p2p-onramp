# P2P Fiat-to-Crypto On-Ramp for Cardano

Trustless P2P on-ramp is a smart contract app that connects cryptocurrency sellers and buyers. A seller deposits funds into the smart contract and ask for a fiat payment. Once the payment is delivered, the buyer can generate a cryptographic proof and unlock the funds from the smart contract.

## End-to-end testing instructions

This repo contains the off-chain code for building and balancing the transactions implementing the buy/sell trading sequence enabled by the P2P on-ramp smart contract.

To execute the end-to-end testing code, make `e2e-test` your active directory.

### Wallet initialization

Code is provided to work with either a local testnet or the public *Preview* testnet.

#### *Preview* testnet

- Execute `./p2p/preview-ini.sh`.
- Fund Alice's wallet (address displayed)

#### Local testnet

Simply execute `./p2p/local-ini.sh`.

(Instructions for setting up a local testnet can be found [here](https://github.com/zkFold/zkfold-cardano/tree/main/e2e-test).)

### Buy/sell trading sequence

To reproduce the buy/sell sequence of transactions, execute:

- `./p2p/01-p2p-init-transaction.sh`  serializes the `onRamp` plutus script and initializes the sellers' and buyer's wallets.
- `./p2p/02-p2p-add-sellers.sh`  adds sell offers to the onRamp script.
- `cardano-cli conway query utxo --address $(cat ./p2p/keys/onRamp.addr) --testnet-magic 2`  verify sell offers are onchain
- `./p2p/03-p2p-buy-order.sh`  selects a "best" offer and updates the corresponding datum with a buy order.
- `./p2p/04-p2p-claim-transaction`  buyer claims the funds after fiat witness provides cryptographic proof.
- `./p2p/05-p2p-cancel-transaction.sh`  sellers whose orders were not executed can reclaim funds after deadline has passed.
- `./p2p/reset.sh`  erases keys and assets to **reset the system**.

Notes:

- Make sure to reset system before trying the sequence again.
- For third step, magic number is 2 for preview testnet and 42 for local testnet.

## Documentation

For details, see document [progress_report.pdf](https://github.com/zkFold/p2p-onramp/raw/main/progress_report.pdf).
