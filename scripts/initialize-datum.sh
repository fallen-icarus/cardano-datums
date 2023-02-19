# Variables
dir="../assets/datum-files/"
tmpDir="../assets/tmp/"

helperScriptFile="${dir}helper.plutus"
helperAddrFile="${dir}helper.addr"

datumFile="../test.json" # This is your datum file

# Export the helper script
cardano-datums export-helper-script \
  --out-file $helperScriptFile

# Create the helper script address using your staking credential
cardano-cli address build \
  --payment-script-file $helperScriptFile \
  --stake-verification-key-file "../assets/wallets/01Stake.vkey" \
  --testnet-magic 1 \
  --out-file $helperAddrFile

# Create transaction
cardano-cli transaction build \
  --tx-in 21cdd4bf4835c84a907c48a949dc0e1c8d30ec2d1577664a639fd611b661334c#0 \
  --tx-out "$(cat ${helperAddrFile}) + 1234567 lovelace" \
  --tx-out-datum-hash-file $datumFile \
  --change-address $(cat ../assets/wallets/01.addr) \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"