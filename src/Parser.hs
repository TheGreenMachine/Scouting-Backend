module Parser (
  parseTeam,
  parseMatch
  ) where
import Safe
import Control.Monad (mapM, liftM, liftM2) -- When do I not import this?
import GameData
import Data.String.Utils

type TeamCSV = String
type TeamNum = Int
---------------------- Team Parser -------------------------------
-- | Takes a csv file representing a team and parses it to a TeamInfo. If any errors occur, Nothing is returned.
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
  let fields = map (split "|"). lines $ csv
  mapM (\line -> atMay line 0 >>= readMay) fields

getAuto :: TeamCSV -> Maybe Double
getAuto = averageField 1

getMain :: TeamCSV -> Maybe Double
getMain = averageField 2

checkClimb :: TeamCSV -> Maybe Bool
checkClimb csv = do
  let fields = map (split "|") . lines $ csv
  scores <- mapM (\line -> atMay line 3 >>= readMay) fields :: Maybe [Int]
  return $ sum scores > 0

getClimb :: TeamCSV -> Maybe Double
getClimb = averageField 4

getPenalties :: TeamCSV -> Maybe [String]
getPenalties csv = do
  let fields = map (split "|") . lines $ csv
  penalties <-  mapM
    (`atMay` line 5)
    fields
  return $ filter (/="none") penalties

averageField :: Int -> TeamCSV -> Maybe Double
averageField n csv = do
  let fields = map (split "|") . lines $ csv
  scores <- mapM (\line -> atMay line n >>= readMay ) fields :: Maybe [Double]
  return $ sum scores / fromIntegral (length scores)

---------------------------- Match Parser --------------------------------------
type MatchNum = Int
type MatchCSV = String
-- | Similar to parseTeam but instead parses a csv representing a match
parseMatch :: MatchNum -> MatchCSV -> Maybe MatchInfo
parseMatch num csv = do
  let [firstAlliance, secondAlliance] = lines csv
  liftM MatchInfo $ liftM2 (,) (parseAlliance firstAlliance) (parseAlliance secondAlliance)
  where
    parseAlliance line = do
      let [color, team1, team2, team3, score] = split "|"  line
      c <- readMay color >>= \n -> return (if n == 0 then Blue else Red)
      return $ Alliance (show num) c team1 team2 team3 score