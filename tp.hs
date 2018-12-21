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
            | Line Color (Int, Int) [(Int,Int)] deriving(Show)

data Screen = Screen {
  width::Int,
  height::Int,
  shapes::[Shape]
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
convert (Line color (x, y) l) = "<path d=\"M "++(show x)++" "++(show y)++" "++(getLines l)++"\" stroke=\""++(getColor color)++"\" stroke-width=\"3\" fill=\"none\"/>"

-- get string line
getLines::[(Int, Int)] -> String
getLines [] = ""
getLines ((x,y):ls) = "l "++(show x)++" "++(show y)++" "++(getLines ls)

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

main::IO()
main = do
  -- print $ myRandom 100
  -- print $ addShape(Screen 1000 1000 []) (Circle (Color (RGB 255 100 12)) (80,150) 80)
  export (aleaRec (emptyScreen 1000 1000 ) 10) "tp_tutrle_first_part.html" 
  export (Screen 1000 1000 [c,r,l]) "first_test.html"
  where c = (Circle (Color (RGB 255 100 12)) (80,150) 80)
        r = (Rect Blue (140,200) 60 50)
        l = (Line Yellow (200,300) [(50,80), (-60,10), (20,-70)])
