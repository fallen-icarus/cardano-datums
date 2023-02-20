# Variables
dir="../assets/datum-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacon.plutus"
beaconRedeemerFile="${dir}mint.json"

datumFile="../test.json"  # This is your datum file - just needed for token name

# Calculate the hash of the datum to be added - needed for token name
datumHash=$(cardano-cli transaction hash-script-data \
  --script-data-file $datumFile)

# Export the beacon policy
cardano-datums export-policy \
  --out-file $beaconPolicyFile

# Create the beacon redeemer
cardano-datums redeemer \
  --burn-beacon \
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
  --tx-in dca07939aca91e3c46ca8ef0da62608c7f091fe54284604ed3083495eb45f6fa#1 \
  --tx-in dca07939aca91e3c46ca8ef0da62608c7f091fe54284604ed3083495eb45f6fa#0 \
  --mint "-1 ${beacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address $(cat ../assets/wallets/01.addr) \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"