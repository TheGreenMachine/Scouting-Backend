{-# LANGUAGE OverloadedStrings #-}
module Match where
import System.Directory
import GameData
import Control.Monad
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Prelude hiding (round)

genMatchPages :: [MatchInfo] -> IO ()
genMatchPages = mapM_ makePage

makePage :: MatchInfo -> IO ()
makePage match = do
  let html = template match
  let MatchInfo (a, _) = match
  writeFile ("site/matches/" ++ round a) $ renderHtml html

template :: MatchInfo -> Html
template (MatchInfo (a1, a2)) = docTypeHtml $ do
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
    ul $ mapM_ toHtml [team1Num,team2Num,team3Num]