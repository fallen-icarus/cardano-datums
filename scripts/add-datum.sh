# Variables
dir="../assets/datum-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacon.plutus"
beaconRedeemerFile="${dir}mint.json"

helperScriptFile="${dir}helper.plutus"

datumFile="../test.json"  # This is your datum file

# Calculate the hash of the datum to be added - needed for token name
datumHash=$(cardano-cli transaction hash-script-data \
  --script-data-file $datumFile)

# Export the beacon policy
cardano-datums export-policy \
  --out-file $beaconPolicyFile

# Create the beacon redeemer
cardano-datums redeemer \
  --mint-beacon $datumHash \
  --out-file $beaconRedeemerFile

# Get the beacon policy id
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

# Helper beacon variable
beacon="${beaconPolicyId}.${datumHash}"

# Create the transaction
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in dcea2eb1f75a1e7bfdb0139fb1c034a1c2a6dddadab63c492da8955196957089#1 \
  --tx-in dcea2eb1f75a1e7bfdb0139fb1c034a1c2a6dddadab63c492da8955196957089#0 \
  --tx-in-script-file $helperScriptFile \
  --tx-in-datum-file $datumFile \
  --tx-in-redeemer-file $datumFile \
  --tx-out "$(cat ../assets/wallets/01.addr) + 2000000 lovelace + 1 ${beacon}" \
  --tx-out-inline-datum-file $datumFile \
  --mint "1 ${beacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address $(cat ../assets/wallets/01.addr) \
  --required-signer-hash $(cat ../assets/wallets/01Stake.pkh) \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --signing-key-file ../assets/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"