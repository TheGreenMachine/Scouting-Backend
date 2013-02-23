module Parser (
  parseTeam
  ) where
import Safe
import Control.Monad (mapM) -- When do I not import this?
import TeamData

type TeamCSV = String
type TeamNum = Int

parseTeam :: TeamNum -> TeamCSV -> Maybe TeamInfo
parseTeam num csv = do
  auto      <- getAuto csv
  main      <- getMain csv
  didClimb  <- checkClimb csv
  climb     <- getClimb csv
  penalties <- getPenalties csv
  matches   <- getMatches csv
  return $ TeamInfo num auto main didClimb climb penalties matches

getMatches :: TeamCSV -> Maybe [Int]
getMatches csv = do
  let fields = map words. lines $ csv
  mapM (\line -> atMay line 0 >>= readMay) fields

getAuto :: TeamCSV -> Maybe Double
getAuto = averageField 1

getMain :: TeamCSV -> Maybe Double
getMain = averageField 2

checkClimb :: TeamCSV -> Maybe Bool
checkClimb csv = do
  let fields = map words . lines $ csv
  scores <- mapM (\line -> atMay line 3 >>= readMay) fields :: Maybe [Int]
  return $ (sum scores) > 0

getClimb :: TeamCSV -> Maybe Double
getClimb = averageField 4

getPenalties :: TeamCSV -> Maybe [String]
getPenalties csv = do
  let fields = map words . lines $ csv
  penalties <-  mapM
    (\line -> atMay line 5)
    fields
  return $ filter (/="none") penalties

averageField :: Int -> TeamCSV -> Maybe Double
averageField n csv = do
  let fields = map words . lines $ csv
  scores <- mapM (\line -> atMay line n >>= readMay ) fields :: Maybe [Double]
  return $ (sum scores) / (fromIntegral $ length scores)