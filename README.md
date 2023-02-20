# Cardano-Datums

A cryptographically verified and distributed data map from datum hashes to their preimages.

---
## Motivation
There are use cases where one needs to know the preimage of a datum's hash. Currently, third parties must be trusted to maintain this mapping in their own databases. It would be better if this mapping could be maintained in a verifiable manner that anyone can access.

This application takes advantage of beacon tokens to create a cryptographically verified mapping from datum hashes to inline datums in a manner that is easily broadcasted to all users. The datum's hash is stored in the beacon token's token name and the token can only be minted if it is being stored in the same UTxO as the associated inline datum. With this, the database would be embeded into the Cardano blockchain itself and verifiable by any one with access to the Cardano blockchain.

---
## The Datum Map Beacon
In order to mint a datum beacon, both the datum's hash and the inline datum must be present in the same transaction. To be specific, a plutus script UTxO with the datum's hash must be spent in the same transaction where the beacon is minted and stored with the inline version for the datum. This spending requirement is the only way to get the datum hash to be accessible to the minting policy in a way where it can verify that the datum hash actually matches the inline datum.

All of the datum beacons use the same policy id. You can think of this as the name of the database. As mentioned before, the token name for each beacon is the associated datum hash. Using these beacons, you can go to their minting transactions and find the outputs where they were stored. That is cryptographically guaranteed to hold the inline datum for that datum hash.

You can then use the following off-chain api services to lookup the inline datum associated with each datum hash:

| Action | Blockfrost | Koios |
|--|--|--|
| Mint Txs | [api](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1history/get) | [api](https://api.koios.rest/#get-/asset_history) |
| Tx Output Info | [api](https://docs.blockfrost.io/#tag/Cardano-Transactions/paths/~1txs~1%7Bhash%7D~1utxos/get) | [api](https://api.koios.rest/#post-/tx_utxos) |

Using this approach, anyone can lookup the preimage of a datum hash without having to rely on a third party's database.

:note: Technically, Blockfrost will still make you use their local mapping of datum hashes to preimages. This is because of the way they allow querying of the UTxO information. This is not true for Koios which allows you to get the inline datums directly from the UTxO information.

Since the minting transaction is the only important part, burning these beacons is always allowed.