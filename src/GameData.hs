module GameData (
  TeamInfo(..),
  MatchInfo (..),
  Alliance (..),
  Color (..),
  matchNum
  ) where
import Data.String.Utils
import Prelude hiding (round)
-- | A data type which is used to represent all known information and averages of about a team.
data TeamInfo = TeamInfo {
  number    :: Int, -- | The team's number (ex 1816)
  auto      :: Double, -- | Points scored on average during autonomous
  main      :: Double, -- | Average points during teleop
  climbed   :: Bool,   -- | Has the team ever attempted to climb
  climb     :: Double, -- | How many points has the team gotten from climbing
  penalties :: [String], -- | A list of all penalties given to the team
  matches   :: [Int] -- | A list of all matches the team has played in.
  }
instance Show TeamInfo where
  show (TeamInfo num au ma didClimb cl ps _) = join " " [show num, show au, show ma, show didClimb, show cl, join ":" ps]

data Color = Red | Blue
                   deriving(Eq, Show)
-- | A data type used to represent alliance members and other information during a match
data Alliance = Alliance {
  round :: String, -- | The match number
  color :: Color,  -- | The color of the alliance, which is either Blue or Red
  team1 :: String, -- | Team number of team 1
  team2 :: String, -- | Team number of team 2
  team3 :: String, -- | Team number of team 3
  score :: String  -- | The overall score of the alliance, including all penalties
  } deriving(Show)

-- | MatchInfo is just a nice wrapper to make dealing with the full information of a match
-- (2 alliances) less finger work.
newtype MatchInfo = MatchInfo (Alliance, Alliance)

matchNum :: MatchInfo -> String
matchNum (MatchInfo (a, _)) = round a