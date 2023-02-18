# Variables
tmpDir="../assets/tmp/"

# Create transaction
cardano-cli transaction build \
  --tx-in c1046bf8695532f8a6cf693583a0b27d92688cd2caccdfe66f7ee97963f8d9f1#1 \
  --tx-out "$(cat ../helper.addr) + 2234567 lovelace" \
  --tx-out-datum-hash-file ../test.json \
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