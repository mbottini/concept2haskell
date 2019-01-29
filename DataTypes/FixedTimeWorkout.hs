module DataTypes.FixedTimeWorkout where

import qualified DataTypes.TableEntry as Te
import qualified DataTypes.FixedHeader as Fh
import qualified DataTypes.TimeFrame as Tf

data FixedTimeWorkout = FixedTimeWorkout {
    tableEntry :: Te.TableEntry,
    header :: Fh.FixedHeader,
    frames :: [Tf.TimeFrame]
} deriving(Show)

