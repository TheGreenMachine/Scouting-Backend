module GameLoader where
import System.Directory
import System.FilePath.Posix
import Control.Monad
import Control.Applicative
import GameData
import Parser

loadTeams :: IO [TeamInfo]
loadTeams = loadInfo computeInfo "teams/"
  where
    computeInfo pairs =
      case pairs of
        (num, csv):rest -> case parseTeam num csv of
          Just info -> liftM (info:) $ computeInfo rest
          Nothing   -> putStrLn ("Failed to parse team "++show num) >> computeInfo rest
        []              -> return []


loadMatches :: IO [MatchInfo]
loadMatches = loadInfo computeInfo "matches/"
  where
    computeInfo pairs =
      case pairs of
        (num, csv):rest -> case parseMatch num csv of
          Just info -> liftM (info:) $ computeInfo rest
          Nothing   -> putStrLn ("Failed to parse match "++show num) >> computeInfo rest
        []              -> return []
loadInfo :: ([(Int, String)]-> IO [a]) -> FilePath -> IO [a]
loadInfo parser path = do
  fileList <- liftM (filter $ (&&)<$>(/= ".")<*>(/="..")) .  getDirectoryContents $ path
  let teamNums = map (read . dropExtension) fileList :: [Int]
  mapM (readFile . (path++)) fileList >>= parser . zip teamNums