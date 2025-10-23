# P2P Fiat-to-Crypto On-Ramp for Cardano

Trustless P2P on-ramp is a smart contract app that connects cryptocurrency sellers and buyers. A seller deposits funds into the smart contract and ask for a fiat payment. Once the payment is delivered, the buyer can generate a cryptographic proof and unlock the funds from the smart contract.

Prototype web-app can be tested at https://p2p.zkfold.io/

## Web-app instructions

The app allows for "sellers" to post sell orders (crypto for fiat) and for "buyers" to choose and buy a sell order.  The web-app contains four sections:
1) Wallet selector
2) Seller's panel
3) Listing of sell orders
4) Buyer's panel

A seller operates with sections 1, 2, and 3; the buyer with sections 1, 3 and 4.

### Seller's panel

- **Posting a sell order.**  The seller commits to a *sell offer* by filling the required fields and pressing the **Sell Offer** button.  This prompts the seller to sign a message (needed to collect the seller's public key).  Next, the seller presses button **Submit Tx** to sign and submit the transaction that sends the seller's offered crypto, together with the corresponding sell-offer's datum, to the p2p-onramp smart contract.  The seller should take note of the *OrderID* associated to his/her offer.

- **Cancelling a sell order.**  If after a while there is no buyer for the seller's offer, the seller can cancel the corresponding order by entering its OrderID and pressing the **Cancel Order** button.  This allows the seller to recover the offered crypto.  If a buyer has commited to the seller's offer, but has not deposited the fiat within the "claim grace period", the seller can go ahead and cancel the order.

### Listing of sell orders

Press the **Available sell orders** button to refresh the list of available sell orders.  Those that have a commited buyer are highlighted.

### Buyer's panel

The buyer commits to a sell order by entering the corresponding OrderID and pressing the *Sign Tx* button.  The corresponding submitted transaction updates the order with the buyer's pub-key-hash and a time-lock associated to the "claim grace period" for the buyer to deposit the fiat.  Next, the buyer presses the **Verify fiat** button for the platfrom to "verify the fiat deposit" and generating the corresponding cryptographic signature.  Finally, the buyer presses the **Claim crypto** button to claim the bought crypto.

## Installation instructions

If you so desire, you can run the P2P-OnRamp web app locally.

### Requirements

Compilation was tested with GHC 9.6.6 and Cabal 3.10.2.1.  Other library requirements are described in [this](https://github.com/input-output-hk/cardano-node-wiki/blob/602fe3a56a13a773cd6c0e00420ee3e5c56f2857/docs/getting-started/install.md) guide.  Additionally, `libpq-dev` or `postgresql` need to be installed as otherwise an error suggesting missing pg_config can occur.

### p2p-onramp server

To run the p2p-onramp server, execute:
```shell
cabal run p2p-server -- provider-config.json onramp-config.json
```
where
1. `provider-config.json` contains your configuration for network and provider.  File `maestro-config-TEMPLATE.json` provides a template configuration.
2. `onramp-config.json` contains the P2P-OnRamp's configuration parameters.  File `onramp-config-TEMPLATE.json` provides a guide for the platform's configuration.

*Note:*  File `onramp-config-TEMPLATE.json` makes reference to files `fiat.addr`, `fiat.vkey` and `fiat.skey`.  These can be generated with `cardano-cli` as usual:
```shell
cardano-cli conway address key-gen --verification-key-file fiat.vkey --signing-key-file fiat.skey
cardano-cli conway address build --payment-verification-key-file fiat.vkey --out-file fiat.addr --testnet-magic 2
```

The p2p-onramp server on this repository was written using the [Atlas](https://atlas-app.io) framework.

### p2p-onramp client

The p2p-onramp frontend can be found [here](https://github.com/zkFold/p2p-onramp-client).

## Test suite

A comprehensive test suite for the *P2P-onramp* smart contract can be found in directory [./tests](./tests).  To run the tests, execute:
```
cabal test
```

