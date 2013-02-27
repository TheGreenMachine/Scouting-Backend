{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Website.Match where
import System.Directory
import GameData
import Website.Template
import Control.Monad
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Prelude hiding (round)

genMatchPages :: PartialToken -> IO ()
genMatchPages partial = mapM_ (makePage token) $ matchList token
                        where token = partial "../"

makePage :: TemplateToken -> MatchInfo -> IO ()
makePage token match = do
  let html = template token match
  writeFile ("site/matches/" ++ matchNum match  ++ ".html") $ renderHtml html
  putStrLn $ "Generate page for match " ++ matchNum match

template :: TemplateToken -> MatchInfo -> Html
template token (MatchInfo (a1, a2)) = wrapTemplate token $ do
  H.head $ do
    H.title . toHtml $ "Match number " ++ (round a1)
  body $ do
    h1 . toHtml $ "Match number " ++ (round a1)
    allTemplate a1
    allTemplate a2

allTemplate :: Alliance -> Html
allTemplate (Alliance roundNum
          colorT
          team1Num
          team2Num
          team3Num
          scoreT) = docTypeHtml $ do
  body $ do
    h2 . toHtml $ show colorT
    h3.toHtml $ "Score: "++scoreT
    h3 "Teams"
    ul $ mapM_ (\num -> li $ a ! href (toValue $ "../"++num++".html") $ toHtml num) [team1Num,team2Num,team3Num]