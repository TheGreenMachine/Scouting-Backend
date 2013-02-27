module Website where
import GameLoader
import GameData
import Website.Match
import Website.Team
import Website.Index
import Website.Template
import System.Directory
import Data.List
import Data.Function
import Data.Ord

-- | This is the toplevel function that should be used to generate a site.
-- It will create a site and site/matches folder if none exists, then move all images over to it and
-- generate all the team and match pages.
generateSite :: [TeamInfo] -> IO ()
generateSite teams = do
  matches <- loadMatches
  createDirectoryIfMissing True "site/matches"
  let partialToken = TemplateToken
                     (sortBy (compare `on` average) teams)
                     (sortBy (compare `on` matchNum) matches)
  putStrLn "Generating Match Pages"
  genMatchPages partialToken
  putStrLn "Generating Team Pages"
  genTeamPages partialToken
  putStrLn "Generating Index"
  genIndex partialToken
  where
    average team = (sum . map ($team) $ [auto, main, climb]) / 3
