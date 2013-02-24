module Website where
import GameLoader
import Match
import Team
import System.Directory
import GameData
generateSite :: [TeamInfo] -> IO ()
generateSite teams = do
  createDirectoryIfMissing True "site/matches"
  matches <- loadMatches
  putStrLn "Generating Match Pages"
  genMatchPages matches
  putStrLn "Generating Team Pages"
  genTeamPages teams
