module DataTypes.FixedDistanceWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.FixedHeader as Fh
import qualified DataTypes.DistanceFrame as Df

data FixedDistanceWorkout = FixedDistanceWorkout {
    tableEntry :: Te.TableEntry,
    header :: Fh.FixedHeader,
    frames :: [Df.DistanceFrame]
} deriving(Show)

