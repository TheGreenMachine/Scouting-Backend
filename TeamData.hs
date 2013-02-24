module TeamData (
  TeamInfo(..)
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