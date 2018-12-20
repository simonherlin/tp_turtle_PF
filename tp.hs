import System.IO.Unsafe
import System.Random

data Color = Color RGB 
            | Red 
            | Blue 
            | Green 
            | Black 
            | White 
            | Yellow 
            | Orange deriving(Show)
data RGB = RGB Int Int Int deriving(Show)
data Shape = Circle Color (Int, Int) Int 
            | Rect Color (Int, Int) Int Int 
            | Line Color (Int, Int) [(Int,Int)] deriving(Show)

data Screen = Screen {
  width::Int,
  height::Int,
  shapes::[Shape]
} deriving(Show)

getColor::Color -> String
getColor Red = (getRGB (RGB 255 0 0))
getColor Blue = (getRGB (RGB 0 0 255))
getColor Green = (getRGB (RGB 0 255 0))
getColor Black = (getRGB (RGB 0 0 0))
getColor White = (getRGB (RGB 255 255 255))
getColor Yellow = (getRGB (RGB 255 255 0))
getColor Orange = (getRGB (RGB 255 140 0))
getColor (Color rgb) = (getRGB rgb)

getRGB::RGB -> String
getRGB (RGB r g b) = "rgb("++(show r)++","++(show g)++","++(show b)++")"

convert::Shape -> String
convert (Circle color (x,y) r) = "<circle cx=\""++(show x)++"\" cy=\""++(show y)++"\" r=\""++(show r)++"\" fill=\""++(getColor color)++"\"/>"
convert (Rect color (x, y) w h) = "<rect x=\""++(show x)++"\" y=\""++(show y)++"\" width=\""++(show w)++"\" height=\""++(show h)++"\"  fill=\""++(getColor color)++"\"/>"
convert (Line color (x, y) l) = "<path d=\"M "++(show x)++" "++(show y)++" "++(getLines l)++"\" stroke=\""++(getColor color)++"\" stroke-width=\"3\" fill=\"none\"/>"

getLines::[(Int, Int)] -> String
getLines [] = ""
getLines ((x,y):ls) = "l "++(show x)++" "++(show y)++" "++(getLines ls)

-- construction doc html d'un coup
docHTML::Screen -> String
docHTML (Screen w h shapes) = "<html><head><title>TP turtle</title></head><body><svg width=\""++(show w)++"\" height=\""++(show h)++"\">"++(foldl (++) "" (map convert shapes))++"</svg></body></html>"
-- premier export 
export::Screen -> IO()
export screen = writeFile "tp_turtle.html" (docHTML screen)


-- construction doc en plusieur morceau
-- beginDocHTML = "<html><head><title>TP turtle</title></head><body>"
-- endDocHTML = "</body></html>"
-- export avec rect alea
-- newExport::Screen -> Int -> IO()
-- newExport screen a = writeFile "tp_turtle2.html" (beginDocHTML ++ (aleaRec a "" screen) ++ endDocHTML)
-- alea rec
-- aleaRec::Int -> String -> Screen -> String
-- aleaRec 0 a _ = return a
-- alea x a (Screen w h _)= aleaRec (x - 1) (a ++ "<svg width=\""++(show w)++"\" height=\""++(show h)++"\">"(Rect Blue ((show randomRIO (0, 100 :: Int),(show randomRIO (0, 100 :: Int)) (show randomRIO (0, 100 :: Int) (show randomRIO (0, 100 :: Int))++"<\svg>") (Screen w h _)

--new fonciton alea rec
newAleaRec::Screen -> Int -> Screen
newAleaRec screen 0 = screen
newAleaRec screen a = newAleaRec (addShape screen (Rect (Color (RGB (myRandom 255) (myRandom 255) (myRandom 255))) ((myRandom 1000),(myRandom 1000)) (myRandom 100) (myRandom 100))) (a - 1)

myRandom :: Int -> Int
myRandom n = unsafePerformIO (getStdRandom (randomR (0, n)))

emptyScreen w h = Screen w h []

addShape::Screen -> Shape -> Screen
addShape (Screen w h shapes) shape = Screen w h (shape:shapes)

-- main::IO()
main = do
  -- print $ myRandom 100
  -- print $ addShape(Screen 1000 1000 []) (Circle (Color (RGB 255 100 12)) (80,150) 80)
    -- putStr . show =<< randomRIO (0, 100 :: Int)
    -- putStr ", "
    -- print         =<< randomRIO (0, 100 :: Int)
  export (newAleaRec (emptyScreen 1000 1000) 10)
  -- export (Screen 1000 1000 [c,r,l])
  -- where c = (Circle (Color (RGB 255 100 12)) (80,150) 80)
  --       r = (Rect Blue (140,200) 60 50)
  --       l = (Line Yellow (200,300) [(50,80), (-60,10), (20,-70)])
  -- newExport (Screen 1000 1000 [aleaRec 10 ""])
