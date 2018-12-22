import System.IO.Unsafe
import System.Random

{-- 

  DATA

--}
data Color = Color RGB 
            | Red 
            | Blue 
            | Green 
            | Black 
            | White 
            | Yellow 
            | Orange 
            | Random deriving(Show)

data RGB = RGB Int Int Int deriving(Show)

data Shape = Circle Color (Int, Int) Int 
            | Rect Color (Int, Int) Int Int 
            | Line Color (Int, Int) (Int,Int) deriving(Show)

data Screen = Screen {
  width::Int,
  height::Int,
  shapes::[Shape]
} deriving(Show)


data Turtle = Turtle {
  x::Int,
  y::Int,
  orientation::Float,
  position::Bool
} deriving(Show)

{--

  Function

--}

-- get String RGB by COlor 
getColor::Color -> String
getColor Red = (getRGB (RGB 255 0 0))
getColor Blue = (getRGB (RGB 0 0 255))
getColor Green = (getRGB (RGB 0 255 0))
getColor Black = (getRGB (RGB 0 0 0))
getColor White = (getRGB (RGB 255 255 255))
getColor Yellow = (getRGB (RGB 255 255 0))
getColor Orange = (getRGB (RGB 255 140 0))
getColor Random = (getRGB (RGB (myRandom 255) (myRandom 255) (myRandom 255)))
getColor (Color rgb) = (getRGB rgb)

-- get string RGB
getRGB::RGB -> String
getRGB (RGB r g b) = "rgb("++(show r)++","++(show g)++","++(show b)++")"

-- get string shape
convert::Shape -> String
convert (Circle color (x,y) r) = "<circle cx=\""++(show x)++"\" cy=\""++(show y)++"\" r=\""++(show r)++"\" fill=\""++(getColor color)++"\"/>"
convert (Rect color (x, y) w h) = "<rect x=\""++(show x)++"\" y=\""++(show y)++"\" width=\""++(show w)++"\" height=\""++(show h)++"\"  fill=\""++(getColor color)++"\"/>"
convert (Line color (x, y) (x2, y2)) = "<path d=\"M "++(show x)++" "++(show y)++" l "++(show x2)++" "++(show y2)++"\" stroke=\""++(getColor color)++"\" stroke-width=\"3\" fill=\"none\"/>"

-- build doc html with Screen containing Shapes
docHTML::Screen -> String
docHTML (Screen w h shapes) = "<html><head><title>TP turtle</title></head><body><svg width=\""++(show w)++"\" height=\""++(show h)++"\">"++(foldl (++) "" (map convert shapes))++"</svg></body></html>"

-- Export screen on HTML document 
export::Screen -> String -> IO()
export screen name = writeFile name (docHTML screen)

-- Function alea REC
aleaRec::Screen -> Int -> Screen
aleaRec screen 0 = screen
aleaRec screen a = aleaRec (addShape screen (Rect Random ((myRandom 1000),(myRandom 1000)) (myRandom 100) (myRandom 100))) (a - 1)

-- Random int in range
myRandom :: Int -> Int
myRandom n = unsafePerformIO (getStdRandom (randomR (0, n)))

-- Create a empty screen
emptyScreen::Int -> Int -> Screen
emptyScreen w h = Screen w h []

-- add shape in screen
addShape::Screen -> Shape -> Screen
addShape (Screen w h shapes) shape = Screen w h (shape:shapes)

{--

Parts 2 Move turtle

--}

-- create Turle
turtleBegin::Turtle
turtleBegin = (Turtle 500 500 0 False)

-- change position
changePosition::Turtle -> Bool -> Turtle
changePosition (Turtle x y orientation position) bool = (Turtle x y orientation bool)

-- change orientation
changeOrientation::(Turtle, Screen) -> Float -> (Turtle, Screen)
changeOrientation ((Turtle x y orientation position), (Screen w h shapes)) a = ((Turtle x y (orientation + a) position), (Screen w h shapes))

-- calcul of new position y
calculY::Int -> Float -> Integer
calculY dist rad = toInteger (round ((sin rad) * (fromInteger (toInteger dist))))

-- calcul of new position x
calculX::Int -> Float -> Integer
calculX dist rad = toInteger (round ((cos rad) * (fromInteger (toInteger dist))))

-- forward turtle
forward::(Turtle, Screen) -> Int -> (Turtle, Screen)
forward ((Turtle x y orientation position), (Screen w h shapes)) a 
    | position == True = (t, (addShape(Screen w h shapes ) s))
    | otherwise = (t, (Screen w h shapes))
      where
          newX = fromIntegral (calculX a orientation)
          newY = fromIntegral (calculY a orientation)
          t = (Turtle (x + newX) (y + newY) orientation position)
          s = Line Red (x, y) (newX, newY)

-- back forward
backForward::(Turtle, Screen) -> Int -> (Turtle, Screen)
backForward ((Turtle x y orientation position), (Screen w h shapes)) a = changeOrientation (forward (changeOrientation((Turtle x y orientation position), (Screen w h shapes)) pi) a) pi

{--

Parts 3

--}

-- draw rectangle with turtle
drawRec::(Turtle, Screen) -> Int -> Int -> (Turtle, Screen)
drawRec ((Turtle x y orientation position), (Screen w h shapes)) cote 0 = ((Turtle x y orientation position), (Screen w h shapes))
drawRec ((Turtle x y orientation position), (Screen w h shapes)) cote a = drawRec (changeOrientation (forward ((Turtle x y orientation position), (Screen w h shapes)) cote) (pi/2)) cote (a - 1)

-- draw circle with turtle
-- drawCircle::(Turtle, Screen) -> Int -> (Turtle, Screen)

-- define number Side and angle
-- defineAngle::Int -> Float
-- defineAngle numberSide = pi/numberSide

-- fonction draw polygon
drawPolygon::(Turtle, Screen) -> Int -> Float -> Int -> (Turtle, Screen)
drawPolygon ((Turtle x y orientation position), (Screen w h shapes)) side angle 0 = ((Turtle x y orientation position), (Screen w h shapes))
drawPolygon ((Turtle x y orientation position), (Screen w h shapes)) side angle numberSide = drawPolygon (changeOrientation (forward ((Turtle x y orientation position), (Screen w h shapes)) side) angle) side angle (numberSide - 1)

main::IO()
main = do
-- Draw circle with polygone function
  export s2 "draw_polygon.html"
    where
      t = changePosition turtleBegin True
      s = emptyScreen 1000 1000
      (turtle, s2) = drawPolygon (t, s) 5 (2 * pi / 300) 300

-- Draw rec first part of part 3
  -- export s2 "draw_rectangle.html"
  --   where
  --     t = changePosition turtleBegin True
  --     s = emptyScreen 1000 1000
  --     width = 300
  --     numberCoteRec = 4
  --     (turtle, s2) = drawRec (t, s) width numberCoteRec

  -- partie 2
  -- export s2 "TORTUE.html"
  --   where
  --     t = changePosition turtleBegin True
  --     s = emptyScreen 1000 1000
  --     m1 = forward (t, s) 30
  --     m2 = changeOrientation m1 (pi/2)
  --     m3 = forward m2 50
  --     m4 = changeOrientation m3 (pi/2)
  --     m5 = forward m4 100
  --     m6 = changeOrientation m5 (pi/2)
  --     (turtle, s2) = backForward m6 30

  -- partie 1 avec test
  -- print $ addShape((emptyScreen 1000 1000) (Line Red (x, y) (newX, newY)))
  -- print $ addShape(Screen 1000 1000 []) (Circle (Color (RGB 255 100 12)) (80,150) 80)
  -- print $ myRandom 100
  -- export (aleaRec (emptyScreen 1000 1000 ) 10) "tp_tutrle_first_part.html" 
  -- export (Screen 1000 1000 [c,r,l]) "first_test.html"
  -- where c = (Circle (Color (RGB 255 100 12)) (80,150) 80)
  --       r = (Rect Blue (140,200) 60 50)
  --       l = (Line Yellow (200,300) (50,80))

--   -- print $ changeOrientation (turtleBegin) 2
--   -- print $ changePosition (turtleBegin) True
