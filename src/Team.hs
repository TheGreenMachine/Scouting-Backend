{-# LANGUAGE OverloadedStrings #-}
module Team where
import System.Directory
import GameData
import Control.Monad
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

genTeamPages :: [TeamInfo] -> IO ()
genTeamPages = mapM_ makePage

makePage :: TeamInfo -> IO ()
makePage team = do
  let image = show (number team) ++ ".jpg"
  hasImage <- doesFileExist $ "images/" ++ image
  if hasImage
     then copyFile ("images/"++image) ("site/"++image)
     else putStrLn $ "No image was found for " ++ show (number team)
  comments <- catch
              (readFile (show $ number team)) $
              \_ -> return ""
  let html = template hasImage comments team
  writeFile ("site/"++(show $ number team)++".html") $ renderHtml html
  putStr "Wrote out page for team "
  print $ number team

template :: Bool -> String -> TeamInfo -> Html
template hasImage comments (TeamInfo num
          autoScore
          mainScore
          didClimb
          climbScore
          penList
          matchList) = docTypeHtml $ do
  let numString = show num
  H.head $ do
    H.title . toHtml $ "Team "++numString
  body $ do
    h1 . toHtml $ "Team "++numString
    if hasImage
      then img ! src (toValue $ numString++".jpg")
      else img ! src "noimage.jpg"
    h3 "Averages"
    ul $ do
      li . toHtml $ "Autonomous: " ++ show autoScore
      li . toHtml $ "Teleop:     " ++ show mainScore
      li . toHtml $ "Climbing:   " ++ show climbScore
    h3 "Penalties"
    ul $ mapM_ (li . toHtml) penList
    h3 "Misc. Comments"
    p $ toHtml comments
    h3 "Matches"
    ul $ mapM_
      (\n -> li $ a ! href (toValue $ "matches/"++show n++".html") $ (toHtml n)) matchList