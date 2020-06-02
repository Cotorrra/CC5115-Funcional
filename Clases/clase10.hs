
data Day = Monday | Tuesday | Wednesday | Thurday | Friday | Saturday | Sunday deriving (Show, Eq, Ord)

weekDay_PM :: Day -> Bool
weekDay_PM Sunday = False
weekDay_PM Saturday = False
weekDay_PM _ = True

weekDay_2 :: Day -> Bool
weekDay_2 d = d < Saturday

data Direction = North | South | East | West

type Position = (Float, Float)
type Point = Position
type Length = Float  
type Distance = Float

data Shape = Circle Point Length | Rectangle Point Point

shift :: Direction -> Position -> Position
shift North (x,y) = (x,y+1)
shift South (x,y) = (x,y-1)
shift East (x,y) = (x+1,y)
shift West (x,y) = (x-1,y)

moveShape :: Shape -> Direction -> Shape
moveShape (Circle p l) d = (Circle (shift d p) l) 
moveShape (Rectangle p1 p2) d = (Rectangle (shift d p1) (shift d p2))

main :: IO ()
main = pure()