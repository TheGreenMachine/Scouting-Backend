module TeamData (
  TeamInfo(..)
  ) where

data TeamInfo = TeamInfo {
  number    :: Int,
  auto      :: Double,
  main      :: Double,
  climbed   :: Bool,
  climb     :: Double,
  penalties :: [String],
  matches   :: [Int]
  } deriving Show