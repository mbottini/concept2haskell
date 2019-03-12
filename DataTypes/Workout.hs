module DataTypes.Workout where


import qualified DataTypes.TableEntry as Te
import qualified DataTypes.WorkoutType as Wt
import qualified DataTypes.FixedDistanceWorkout as Fdw
import qualified DataTypes.FixedCalorieWorkout as Fcw
import qualified DataTypes.FixedTimeWorkout as Ftw
import qualified DataTypes.DistanceIntervalWorkout as Diw
import qualified DataTypes.TimeIntervalWorkout as Tiw
import qualified DataTypes.VariableIntervalWorkout as Viw
import qualified DataTypes.CalorieIntervalWorkout as Ciw

import Data.Aeson
import Data.Word

data Workout = FixedDistanceWorkout Fdw.FixedDistanceWorkout |
               FixedCalorieWorkout Fcw.FixedCalorieWorkout |
               FixedTimeWorkout Ftw.FixedTimeWorkout |
               DistanceIntervalWorkout Diw.DistanceIntervalWorkout |
               TimeIntervalWorkout Tiw.TimeIntervalWorkout |
               VariableIntervalWorkout Viw.VariableIntervalWorkout |
               CalorieIntervalWorkout Ciw.CalorieIntervalWorkout
               deriving(Show)

getFrames :: Te.TableEntry -> [Word8] -> Workout
getFrames te bs = case Te.workoutType te of
   Wt.FreeRow -> FixedTimeWorkout $ Ftw.getFrames te bs
   Wt.SingleDistance -> FixedDistanceWorkout $ Fdw.getFrames te bs
   Wt.SingleTime -> FixedTimeWorkout $ Ftw.getFrames te bs
   Wt.TimedInterval -> TimeIntervalWorkout $ Tiw.getFrames te bs
   Wt.DistanceInterval -> DistanceIntervalWorkout $ Diw.getFrames te bs
   Wt.VariableInterval -> VariableIntervalWorkout $ Viw.getFrames te bs
   Wt.SingleCalorie -> FixedCalorieWorkout $ Fcw.getFrames te bs
   Wt.CalorieInterval -> CalorieIntervalWorkout $ Ciw.getFrames te bs

instance ToJSON Workout where
    toJSON (FixedDistanceWorkout w) = toJSON w
    toJSON (FixedTimeWorkout w) = toJSON w
    toJSON (TimeIntervalWorkout w) = toJSON w
    toJSON (DistanceIntervalWorkout w) = toJSON w
    toJSON (VariableIntervalWorkout w) = toJSON w
    toJSON (CalorieIntervalWorkout w) = toJSON w
    toJSON (FixedCalorieWorkout w) = toJSON w
    toJSON _ = error "Not implemented yet!"
