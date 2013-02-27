{-# LANGUAGE OverloadedStrings #-}
module Website.Team where
import System.Directory
import GameData
import Website.Template
import Control.Monad
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.IO.Error

genTeamPages :: PartialToken -> IO ()
genTeamPages partial = mapM_ (makePage token) $ teamList token
                       where token = partial "./"

makePage :: TemplateToken -> TeamInfo -> IO ()
makePage token team = do
  let image = show (number team) ++ ".jpg"
  hasImage <- doesFileExist $ "images/" ++ image
  if hasImage
     then copyFile ("images/"++image) ("site/"++image)
     else putStrLn $ "No image was found for " ++ show (number team)
  comments <- catchIOError
              (readFile (show $ number team)) $
              \_ -> return ""
  let html = template token hasImage comments team
  writeFile ("site/"++(show $ number team)++".html") $ renderHtml html
  putStr "Wrote out page for team "
  print $ number team

template :: TemplateToken -> Bool -> String -> TeamInfo -> Html
template token hasImage comments (TeamInfo num
          autoScore
          mainScore
          didClimb
          climbScore
          penList
          matchList) = wrapTemplate token $ do
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