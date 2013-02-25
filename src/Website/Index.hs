{-# LANGUAGE OverloadedStrings #-}
module Website.Index where
import Control.Monad
import Data.List
import Data.Ord
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import GameData
genIndex :: [TeamInfo] -> IO ()
genIndex teams = do
  let orderedList = sortBy (comparing snd) $ zip teams (map average teams)
  let html = template orderedList
  writeFile "site/index.html" $ renderHtml html
  where
    average team =  (sum . map ($team) $ [auto, main, climb]) / 3
template :: [(TeamInfo, Double)] -> Html
template list = docTypeHtml $ do
  H.head $ do
    H.title "Home"
  body $ do
    h1 "Main Team Index"
    table ! customAttribute "border" "1" $ do
      th "Name"
      th "Average"
      th "Autonomous"
      th "Teleop"
      th "Climbing"
      mapM_
        (\(info, avg) -> tr $ do
                         td (a ! href (toValue $ show (number info) ++ ".html") $ toHtml $ number info)
                         td . toHtml $ (truncate avg :: Int)
                         td . toHtml $ (truncate $ auto  info :: Int)
                         td . toHtml $ (truncate $ main  info :: Int)
                         td . toHtml $ (truncate $ climb info :: Int))
        list
  where showTuple (a, b) = show a ++ " : " ++ show b
