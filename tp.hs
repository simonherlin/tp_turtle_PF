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

data Order = AV Int | TD Float | Repeat Int [Order] deriving(Show)

data Instruction = Orders [Order] | Be String Int deriving(Show)

data Program = Instructions [Instruction] deriving(Show)

data World = World {
  turtle::Turtle,
  screen::Screen
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

-- new change position with screen
changePosition2::World -> Bool -> World
changePosition2 (World (Turtle x y orientation position) (Screen w h shapes)) bool = (World (Turtle x y orientation bool) (Screen w h shapes))

-- change orientation
changeOrientation::World -> Float -> World
changeOrientation (World (Turtle x y orientation position) (Screen w h shapes)) a = (World (Turtle x y (orientation + a) position) (Screen w h shapes))

-- calcul of new position y
calculY::Int -> Float -> Integer
calculY dist rad = toInteger (round ((sin rad) * (fromInteger (toInteger dist))))

-- calcul of new position x
calculX::Int -> Float -> Integer
calculX dist rad = toInteger (round ((cos rad) * (fromInteger (toInteger dist))))

-- forward turtle
forward::World -> Int -> World
forward (World (Turtle x y orientation position) (Screen w h shapes)) a 
    | position == True = (World t (addShape(Screen w h shapes ) s))
    | otherwise = (World t (Screen w h shapes))
      where
          newX = fromIntegral (calculX a orientation)
          newY = fromIntegral (calculY a orientation)
          t = (Turtle (x + newX) (y + newY) orientation position)
          s = Line Red (x, y) (newX, newY)

-- back forward
backForward::World -> Int -> World
backForward w a = changeOrientation (forward (changeOrientation w pi) a) pi

{--

Parts 3

--}

-- draw rectangle with turtle
drawRec::World -> Int -> Int -> World
drawRec w cote 0 = w
drawRec w cote a = drawRec (changeOrientation (forward w cote) (pi/2)) cote (a - 1)

-- fonction draw polygon
drawPolygon::World -> Int -> Float -> Int -> World
drawPolygon w side angle 0 = w
drawPolygon w side angle numberSide = 
  drawPolygon 
    (changeOrientation (forward w side) angle) 
    side 
    angle 
    (numberSide - 1)

-- create wing
drawWing::World -> Int -> World
drawWing w a = changePosition2 (backForward (changePosition2 (drawRec (forward w a) a 4) False) a) True 

-- create moulin
drawMoulin::World -> Int -> World
drawMoulin w 0 = w
drawMoulin w number = drawMoulin (changeOrientation (drawWing w 60) (pi / 2)) (number - 1)

-- flocon von koch
courbvk::World -> Int -> World
courbvk w 0 = forward w 10
courbvk w nb = 
  courbvk
    (changeOrientation
      (courbvk
        (changeOrientation 
          (courbvk 
            (changeOrientation 
              (courbvk w (nb -1)) 
              (pi / 3)
            ) (nb - 1)
          ) 
          (- 2 * pi / 3)
        )
        (nb - 1)
      )
      (pi / 3)
    )
    (nb - 1)

-- flocon von koch
drawFlocon::World -> Int -> World
drawFlocon w 0 = w
drawFlocon w nb = 
  drawFlocon
    (changeOrientation
      (courbvk
        w
        3
      )
      (- 2 * pi / 3)
    )
    (nb - 1)

{--

Parts 4

--}

repeatOrders::World -> [Order] -> Int -> World
repeatOrders w _ 0 = w
repeatOrders w orders n = execOrders (repeatOrders w orders (n-1)) orders

execOrder::World -> Order -> World
execOrder w (Repeat n orders) = repeatOrders w orders n
execOrder w (AV n) = forward w n
execOrder w (TD n) = changeOrientation w n

execOrders::World -> [Order] -> World
execOrders w l = foldl execOrder w l

execProg::World -> Program -> [(String, Int)] -> World
execProg w (Instructions []) _ = w
execProg w (Instructions ((Orders o):xs)) vars = execOrders (execProg w (Instructions xs) vars) o
execProg w (Instructions ((Be var val):xs)) vars = w

initialisationWorld::World
initialisationWorld = World t s
  where
    t = changePosition turtleBegin True
    s = emptyScreen 1000 1000

main::IO()
main = do
  export s8 "instruction.html"
  export s7 "draw_rec_repeat.html"
  export s6 "draw_flocon.html"
  export s5 "draw_moulin.html"
  export s4 "draw_polygon.html"
  export s3 "draw_rectangle.html"
  export s2 "TORTUE.html"
  export (aleaRec (emptyScreen 1000 1000 ) 10) "tp_tutrle_first_part.html"
  export (Screen 1000 1000 [c,r,l]) "first_test.html"
    where
-- partie 1
      c = (Circle (Color (RGB 255 100 12)) (80,150) 80)
      r = (Rect Blue (140,200) 60 50)
      l = (Line Yellow (200,300) (50,80))
-- partie 2
      w = initialisationWorld
      m1 = forward w 30
      m2 = changeOrientation m1 (pi/2)
      m3 = forward m2 50
      m4 = changeOrientation m3 (pi/2)
      m5 = forward m4 100
      m6 = changeOrientation m5 (pi/2)
      (World turtle2 s2) = backForward m6 30
-- Partie 3
-- Draw rec first part 3
      width = 300
      numberCoteRec = 4
      (World turtle3 s3) = drawRec w width numberCoteRec
-- Draw circle with polygon function
      side = 5
      angle = (2 * pi / 300)
      numberSide = 300
      (World turtle4 s4) = drawPolygon w side angle numberSide
-- draw moulin
      w2 = changeOrientation w (pi / 4)
      numberWings = 4
      (World turtle5 s5) = drawMoulin w2 numberWings
-- draw flocon von koch
      (World turtle6 s6) = drawFlocon w 3
-- Partie 4
-- draw rec with repeat
      orders = [(AV 130), (TD (pi / 2))]
      (World turtle7 s7) = repeatOrders w orders 4
-- draw by instruction
      ordersInstruction = [(Repeat 8 [(AV 130), (TD (pi / 4))])]
      instructions = [(Orders ordersInstruction)]
      (World turtle8 s8) = execProg w (Instructions instructions) []
