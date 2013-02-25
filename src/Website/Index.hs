{-# LANGUAGE OverloadedStrings #-}
module Website.Index where
import Control.Monad
import Data.List
import Data.Ord
import Data.Function
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import GameData
genIndex :: [TeamInfo] -> IO ()
genIndex teams = do
  let orderedList = sortBy (rcompare `on` snd)$ zip teams (map average teams)
  let html = template orderedList
  writeFile "site/index.html" $ renderHtml html
  where
    average team =  (sum . map ($team) $ [auto, main, climb]) / 3
    rcompare a b = case compare a b of
      LT -> GT
      GT -> LT
      EQ -> EQ
template :: [(TeamInfo, Double)] -> Html
template list = docTypeHtml $ do
  H.head $ do
    H.title "Home"
  body $ do
    h1 "Main Team Index"
    table ! customAttribute "border" "1"
          ! customAttribute "cellpadding" "10" $ do
      th $ h2 "Rank"
      th $ h2 "Name"
      th $ h2 "Overall"
      th $ h2 "Autonomous"
      th $ h2 "Teleop"
      th $ h2 "Climbing"
      mapM_
        (\((info, avg), rank) -> tr $ do
                         td . toHtml $ rank
                         td (a ! href (toValue $ show (number info) ++ ".html") $ h3 . toHtml $ number info)
                         td . toHtml $ (truncate avg :: Int)
                         td . toHtml $ (truncate $ auto  info :: Int)
                         td . toHtml $ (truncate $ main  info :: Int)
                         td . toHtml $ (truncate $ climb info :: Int))
        (zip list ([1..] :: [Int]))
