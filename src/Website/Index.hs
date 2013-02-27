{-# LANGUAGE OverloadedStrings #-}
module Website.Index where
import Control.Monad
import Data.List
import Data.Ord
import Data.Function
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import GameData
import Website.Template
genIndex :: PartialToken -> IO ()
genIndex partial = do
  let templateGen = map $ \(name, selector) -> template token name .
                                               sortBy
                                                (rcompare `on` selector . fst) $
                                                zip teams (map average teams)
  let [autoIndex, mainIndex, climbIndex, avgIndex] =
        map renderHtml $ templateGen [("Autonomous", auto)
                                    ,("Teleop", main)
                                    ,("Climbing", climb)
                                    ,("Average", average)]
  putStrLn "Writing autonomous index"
  writeFile "site/auto.html"  $ autoIndex
  putStrLn "Writing teleop index"
  writeFile "site/main.html"  $ mainIndex
  putStrLn "Writing climbing index"
  writeFile "site/climb.html" $ climbIndex
  putStrLn "Writing average index"
  writeFile "site/index.html" $ avgIndex
  where
    token        = partial "./"
    teams        = teamList token
    average team = (sum . map ($team) $ [auto, main, climb]) / 3
    rcompare a b = case compare a b of
      LT -> GT
      GT -> LT
      EQ -> EQ
template :: TemplateToken -> String -> [(TeamInfo, Double)] -> Html
template token name list = wrapTemplate token $ do
  H.head $ do
    H.title "Home"
  body $ do
    h1 . toHtml $ name ++ " Team Index"
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
