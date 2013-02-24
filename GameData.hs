module GameData (
  TeamInfo(..),
  MatchInfo (..),
  Alliance (..),
  Color (..)
  ) where
import Data.String.Utils
data TeamInfo = TeamInfo {
  number    :: Int,
  auto      :: Double,
  main      :: Double,
  climbed   :: Bool,
  climb     :: Double,
  penalties :: [String],
  matches   :: [Int]
  }
instance Show TeamInfo where
  show (TeamInfo num au ma didClimb cl ps _) = join " " [show num, show au, show ma, show didClimb, show cl, join ":" ps]

data Color = Red | Blue
                   deriving(Eq, Show)
data Alliance = Alliance {
  round :: String,
  color :: Color,
  team1 :: String,
  team2 :: String,
  team3 :: String,
  score :: String
  } deriving(Show)

newtype MatchInfo = MatchInfo (Alliance, Alliance)