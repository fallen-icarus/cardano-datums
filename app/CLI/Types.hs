module CLI.Types where

import CardanoDatums

data Command 
  = ExportBeaconPolicy !FilePath
  | CreateBeaconRedeemer !BeaconRedeemer !FilePath
  | ExportHelperScript !FilePath
  | QueryDatum !DatumHash !Network !Output

-- | For when saving to file is optional
data Output = Stdout | File !FilePath

data Network 
  = Mainnet String  -- ^ Api key
  | PreProdTestnet String  -- ^ Api key