# Variables
dir="../assets/datum-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacon.plutus"
beaconRedeemerFile="${dir}mint.json"

# Calculate the hash of the datum to be added
datumHash=$(cardano-cli transaction hash-script-data \
  --script-data-file ../test.json)

# Export the beacon policy
cabal run exe:cardano-datums -- export-policy \
  --out-file $beaconPolicyFile

# Create the beacon redeemer
cabal run -v0 exe:cardano-datums -- redeemer \
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
  --tx-in f91c10b841170583a47ac9641ed7eda80166e6199d435e61636ab2d9997aa92c#0 \
  --tx-in f154a582ac89c607d3311ddf70ef774bc03878614dc48200a321bd56258e7bc1#0 \
  --tx-in-script-file ../helper.plutus \
  --tx-in-datum-file ../test.json \
  --tx-in-redeemer-file ../test.json \
  --tx-out "$(cat ../assets/wallets/01.addr) + 2000000 lovelace + 1 ${beacon}" \
  --tx-out-inline-datum-file ../test.json \
  --mint "1 ${beacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address $(cat ../assets/wallets/01.addr) \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"