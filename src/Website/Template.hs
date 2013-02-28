{-# LANGUAGE OverloadedStrings #-}
module Website.Template (
  TemplateToken(..),
  wrapTemplate,
  PartialToken
  ) where
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import GameData
import Data.Function
import Data.List
import Data.Ord
import Prelude hiding (div, round)

-- | A datatype meant to be passed to the wrapTemplate function to construct the menus.
data TemplateToken = TemplateToken {
  teamList  :: [TeamInfo], -- | A sorted list representing all teams
  matchList :: [MatchInfo],-- | A sorted list representing all matches
  path      :: String      -- | The path to the the root of the website relative to the webpage.
  }
-- | A partially applied constructor that lacks the path for a TemplateToken
type PartialToken = String -> TemplateToken

-- | Should take a page and wrap it with a left, right, and top menu displaying the teams, matches, and indices respectively
wrapTemplate :: TemplateToken  -- | The token containing all data for the menus
                -> Html        -- | The HTML to be wrapped.
                -> Html        -- | The wrapped HTML
wrapTemplate token page = docTypeHtml $ do
  link ! rel "stylesheet" ! type_ "text/css" ! href (toValue $ path token ++ "screen.css")
  ul ! A.id "nav-left" $ mapM_ (\team -> li
                       (a ! href (toValue $ path token ++ show (number team) ++ ".html") $ toHtml $ number team)) $
    teamList token
  ul ! A.id "nav-right" $ mapM_ (\(MatchInfo (a1, a2)) -> li
                                       (a ! href (toValue $ path token ++ "matches/" ++ round a1 ++ ".html") $ toHtml (round a1))) $
    matchList token
  ul ! A.id "nav-top" $ do
    li $ a ! href (toValue $ path token ++ "index.html") $ " Average "
    li $ a ! href (toValue $ path token ++ "auto.html")  $ " Autonomous "
    li $ a ! href (toValue $ path token ++ "main.html")  $ " Teleop "
    li $ a ! href (toValue $ path token ++ "climb.html") $ " Climbing "
  div ! customAttribute "class" "main-page" $ page