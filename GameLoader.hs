module GameLoader (
  loadTeams,
  loadMatches
  ) where
import System.Directory
import System.FilePath.Posix
import Control.Monad
import Control.Applicative
import GameData
import Parser

-- | Loads in all teams in the folder 'teams' and returns them as a list of TeamInfo's
loadTeams :: IO [TeamInfo]
loadTeams = loadInfo (computeInfo "Failed to parse team " parseTeam) "teams/"

-- | Loads in all matches in the folder 'matches' and returns them as a list of MatchInfo's
loadMatches :: IO [MatchInfo]
loadMatches = loadInfo (computeInfo "Failed to parse match " parseMatch) "matches/"

loadInfo :: ([(Int, String)]-> IO [a]) -> FilePath -> IO [a]
loadInfo parser path = do
  fileList <- liftM (filter $ (&&)<$>(/= ".")<*>(/="..")) .  getDirectoryContents $ path
  let teamNums = map (read . dropExtension) fileList :: [Int]
  mapM (readFile . (path++)) fileList >>= parser . zip teamNums

computeInfo :: String -> (Int -> String -> Maybe a) -> [(Int, String)] -> IO [a]
computeInfo errorMsg parser pairs =
  case pairs of
    (num, csv):rest -> case parser num csv of
      Just info -> liftM (info:) $ computeInfo errorMsg parser rest
      Nothing   -> putStrLn (errorMsg++show num) >> computeInfo errorMsg parser rest
    []              -> return []