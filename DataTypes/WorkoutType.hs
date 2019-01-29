module DataTypes.WorkoutType where

data WorkoutType = FreeRow |
                   SingleDistance |
                   SingleTime |
                   TimedInterval |
                   DistanceInterval |
                   VariableInterval |
                   SingleCalorie
                   deriving(Show)

parseWorkoutType :: Int -> WorkoutType
parseWorkoutType 1 = FreeRow
parseWorkoutType 3 = SingleDistance
parseWorkoutType 5 = SingleTime
parseWorkoutType 6 = TimedInterval
parseWorkoutType 7 = DistanceInterval
parseWorkoutType 8 = VariableInterval
parseWorkoutType 10 = SingleCalorie
parseWorkoutType n = error ("parseWorkoutType: Invalid number " ++ (show n))



