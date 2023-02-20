# Getting Started

:warning: Assumes a local PreProduction Testnet node running locally and `cardano-cli` installed since it is used to actually build and sign transactions.

Template bash scripts that follow these steps are available [here](scripts/).

---
## Table of Contents
- [Installing](#installing)
- [Initialize the datum to be added](#initialize-the-datum-to-be-added)
- [Add datum to the map](#add-datum-to-map)

---
## Installing
Instructions are adapted from the [plutus-pioneers-program](https://github.com/input-output-hk/plutus-pioneer-program) week 1 exercise.

1. Install NixOS cross-referencing the following resources.
     - https://nixos.org/download.html
     - https://docs.plutus-community.com
     - A few resources to understand the what and why regarding NixOS
       - https://nixos.org/manual/nix/stable
       - https://serokell.io/blog/what-is-nix
2. Set-up IOHK binary caches [How to set up the IOHK binary caches](https://github.com/input-output-hk/plutus-apps#iohk-binary-cache). "If you do not do this, you will end up building GHC, which takes several hours. If you find yourself building GHC, *stop* and fix the cache."

3. After adding the cache, you will need to restart the nix service. This can be done by executing `sudo systemctl restart nix` or by restarting your machine. If the cache was configured properly, you should see a lot of `copying path ... from 'https://cache.iog.io'` when you execute `nix-shell` in the next step.

4. Execute the following:
```
git clone https://github.com/fallen-icarus/cardano-datums
git clone https://github.com/input-output-hk/plutus-apps
cd plutus-apps
git checkout v1.0.0
nix-shell           # this may take a while the first time

# Your terminal should now have a nix-shell prompt

cd ../cardano-datums
cabal clean
cabal update
cabal build all
```
The `cardano-datums` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-datums-0.1.0.0/x/cardano-datums/build/cardano-datums/cardano-datums`. Move the program to somewhere in your $PATH.

You can now exit the nix-shell with `exit`.

All `cardano-datums` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

---
## Initialize the datum to be added
As stated in the README, in order for the minting policy to be able to verify that the datum hash actually matches the inline datum, a script input with the datum's hash must be consumed in the minting transaction. A helper spending script has been provided for this use case. You do not lose custody of the assets stored with the datum when using this script. This script has the following features:

1. It is datum and redeemer agnostic - it can be used with any datum and redeemer.
2. Spending is guarded by the staking credential - if the addresses staking credential is a pubkey, the pubkey must sign the tx; if the staking credential is a script, the script must be executed in the same tx where the UTxO is consumed.

Thanks to number 2, your funds are protected during the initialization step. Number 1 means that you can use the datum you wish to upload as both the datum and redeemer for this helper script.

### Export the helper script
``` Bash
cardano-datums export-helper-script \
  --out-file helper.plutus
```

### Create the helper script address using your script credential (example uses pubkey)
``` Bash
cardano-cli address build \
  --payment-script-file helper.addr \
  --stake-verification-key-file stake.vkey \
  --testnet-magic 1 \
  --out-file helper.addr
```

### Create transaction and submit it
``` Bash
cardano-cli transaction build \
  --tx-in <utxo_for_storing_with_datum_and_paying_fee> \
  --tx-out "$(cat helper.addr) + 1234567 lovelace" \
  --tx-out-datum-hash-file <your_datum_file> \
  --change-address <your_change_address> \
  --testnet-magic 1 \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file <your_payment_skey> \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Add datum to map
### Calculate the hash of the datum to be added - needed for token name
``` Bash
datumHash=$(cardano-cli transaction hash-script-data \
  --script-data-file <your_datum_file>)
```

### Export the beacon policy
``` Bash
cardano-datums -- export-policy \
  --out-file beacon.plutus
```

### Create the beacon redeemer
``` Bash
cardano-datums redeemer \
  --mint-beacon $datumHash \
  --out-file mint.json
```

### Get the beaocn policy id
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus)
```

### Create the full beacon name
``` Bash
beacon="${beaconPolicyId}.${datumHash}"
```

### Create and submit the transaction
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_to_pay_fee> \
  --tx-in <utxo_from_helper_script_addr> \
  --tx-in-script-file helper.plutus \
  --tx-in-datum-file <your_datum_file> \
  --tx-in-redeemer-file <your_datum_file> \
  --tx-out "$(cat <your_personal_addr>) + 2000000 lovelace + 1 ${beacon}" \
  --tx-out-inline-datum-file <your_datum_file> \
  --mint "1 ${beacon}" \
  --mint-script-file beacon.plutus \
  --mint-redeemer-file mint.json \
  --change-address $(cat <your_personal_addr>) \
  --required-signer-hash $(cat stake.vkey) \
  --tx-in-collateral <utxo_for_collateral> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file <your_payment_skey> \
  --signing-key-file stake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Burning the beacon
### Calculate the hash of the datum to be added - needed for token name
``` Bash
datumHash=$(cardano-cli transaction hash-script-data \
  --script-data-file <your_datum_file>)
```

### Export the beacon policy
``` Bash
cardano-datums export-policy \
  --out-file beacon.plutus
```

### Create the beacon redeemer
``` Bash
cardano-datums redeemer \
  --burn-beacon \
  --out-file burn.json
```

### Get the beacon policy id
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus)
```

### Helper beacon variable
``` Bash
beacon="${beaconPolicyId}.${datumHash}"
```

### Create the transaction
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_to_pay_fee> \
  --tx-in <utxo_with_beacon> \
  --mint "-1 ${beacon}" \
  --mint-script-file beacon.plutus \
  --mint-redeemer-file burn.json \
  --change-address $(cat <your_personal_addr>) \
  --tx-in-collateral <utxo_for_collateral> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file <your_payment_skey> \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```