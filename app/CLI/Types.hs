module CLI.Types where

import CardanoDatums

data Command 
  = ExportBeaconPolicy !FilePath
  | CreateBeaconRedeemer !BeaconRedeemer !FilePath
  | ExportHelperScript !FilePath