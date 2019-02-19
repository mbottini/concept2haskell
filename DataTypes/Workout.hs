module DataTypes.Workout where


import qualified DataTypes.TableEntry as Te
import qualified DataTypes.WorkoutType as Wt
import qualified DataTypes.FixedDistanceWorkout as Fdw
import qualified DataTypes.FixedTimeWorkout as Ftw
import qualified DataTypes.DistanceIntervalWorkout as Diw
import qualified DataTypes.TimeIntervalWorkout as Tiw
import qualified DataTypes.VariableIntervalWorkout as Viw

import Data.Word

data Workout = FixedDistanceWorkout Fdw.FixedDistanceWorkout |
               FixedTimeWorkout Ftw.FixedTimeWorkout |
               DistanceIntervalWorkout Diw.DistanceIntervalWorkout |
               TimeIntervalWorkout Tiw.TimeIntervalWorkout |
               VariableIntervalWorkout Viw.VariableIntervalWorkout
               deriving(Show)

getFrames :: Te.TableEntry -> [Word8] -> Workout
getFrames te bs = case Te.workoutType te of
   Wt.FreeRow -> FixedTimeWorkout $ Ftw.getFrames te bs
   Wt.SingleDistance -> FixedDistanceWorkout $ Fdw.getFrames te bs
   Wt.SingleTime -> FixedTimeWorkout $ Ftw.getFrames te bs
   Wt.TimedInterval -> TimeIntervalWorkout $ Tiw.getFrames te bs
   Wt.DistanceInterval -> DistanceIntervalWorkout $ Diw.getFrames te bs
   Wt.VariableInterval -> VariableIntervalWorkout $ Viw.getFrames te bs
   Wt.SingleCalorie -> error "TODO: Figure this out!"