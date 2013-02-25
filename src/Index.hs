{-# LANGUAGE OverloadedStrings #-}
module Index where
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
  let html = template $ map (\(a, b) -> (number a, b)) orderedList
  writeFile "site/index.html" $ renderHtml html
  where
    average team =  (sum . map ($team) $ [auto, main, climb]) / 3
template :: [(Int, Double)] -> Html
template list = docTypeHtml $ do
  H.head $ do
    H.title "Home"
  body $ do
    h1 "Main Team Index"
    ol $ mapM_
      (\num -> li $ a ! href (toValue $ show (fst num) ++ ".html") $ toHtml $ showTuple num) list
  where showTuple (a, b) = show a ++ " : " ++ show b
