module Website where
import GameLoader
import Website.Match
import Website.Team
import Index
import System.Directory
import GameData

-- | This is the toplevel function that should be used to generate a site.
-- It will create a site and site/matches folder if none exists, then move all images over to it and
-- generate all the team and match pages.
generateSite :: [TeamInfo] -> IO ()
generateSite teams = do
  createDirectoryIfMissing True "site/matches"
  matches <- loadMatches
  putStrLn "Generating Match Pages"
  genMatchPages matches
  putStrLn "Generating Team Pages"
  genTeamPages teams
  putStrLn "Generating Index"
  genIndex teams
