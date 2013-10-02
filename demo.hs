
data Shape =
     Circle {radius :: Double}
   | Rectangle {width, height :: Double}
   deriving Show

area :: Shape -> Double
area (Circle r) = 3.1415926535 * r * r
area (Rectangle w h) = w * h

data Pair = P Int Int
    deriving Show

data Color = Red | Orange | Blue | Yellow
    deriving Show

hexColor :: Color -> String
hexColor Red = "#ff0000"
hexColor Orange = "#ff8800"
hexColor Blue = "#0000ff"
hexColor Yellow = "#ffff00"

mult :: Pair -> Int
mult (P a b) = a * b

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)


everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = [x]
everyOther (x:y:xs) = x : everyOther xs
