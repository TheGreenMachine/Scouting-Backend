module TeamLoader where
import System.Directory
import System.FilePath.Posix
import Control.Monad
import Control.Applicative
import TeamData
import Parser

loadTeams :: IO [TeamInfo]
loadTeams = do
  fileList <- liftM (filter $ (&&)<$>(/= ".")<*>(/="..")) .  getDirectoryContents $ "teams"
  let teamNums = map (read . dropExtension) fileList :: [Int]
  mapM (readFile . ("teams/"++)) fileList >>= computeInfo . zip teamNums
  where
    computeInfo pairs =
      case pairs of
        (num, csv):rest -> case parseTeam num csv of
          Just info -> liftM (info:) $ computeInfo rest
          Nothing   -> putStrLn ("Failed to parse team "++show num) >> computeInfo rest
        []              -> return []