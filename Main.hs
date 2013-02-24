module Main where
import TeamLoader
import Data.String.Utils
main = do
  putStrLn "Welcome to the HSB.\nPlease standby..."
  putStrLn "Loading Team Info"
  teamInfos <- loadTeams
  writeFile "teamdata.csv" . join "\n" . map show $ teamInfos --Write the main team info out for AO
  putStrLn "Finished loading team info. CSV was written to 'teamdata.csv'"
  --Build Website--
  putStrLn "Building the Static Website..."