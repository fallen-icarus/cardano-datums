module CLI.Run
(
  runCommand
) where

-- import Data.Aeson
-- import Data.Aeson.Encode.Pretty
-- import qualified Data.ByteString.Lazy as BL

import CardanoDatums
import CLI.Types

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ExportBeaconPolicy file -> exportPolicy file
  CreateBeaconRedeemer r file -> createRedeemer r file
  ExportHelperScript file -> exportHelperScript file

createRedeemer :: BeaconRedeemer -> FilePath -> IO ()
createRedeemer r file = do
  writeData file r
  putStrLn "Beacon redeemer created successfully."

exportPolicy :: FilePath -> IO ()
exportPolicy file = do
  res <- writeScript file beaconScript
  case res of
    Right _ -> putStrLn "Beacon policy exported successfully."
    Left err -> putStrLn $ "There was an error: " <> show err

exportHelperScript :: FilePath -> IO ()
exportHelperScript file = do
  res <- writeScript file spendValidatorScript
  case res of
    Right _ -> putStrLn "Helper script exported successfully."
    Left err -> putStrLn $ "There was an error: " <> show err