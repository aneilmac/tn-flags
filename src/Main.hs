module Main where
import Graphics.WorldTurtle
import Graphics.Gloss.Data.Picture
import Control.Monad

-- Number of flags.
n :: Int
n = 6

-- Flag labels
labels :: [String]
labels = ["e", "s", "ts", "sts"] ++ concat [ ["(ts)^" ++ show i, "s(ts)^" ++ show i] | i <- [2..]]

drawFlag :: Float -> TurtleCommand ()
drawFlag unit = do
    fd 50
    replicateM_ 3 $ fd 30 >> lt (120 * unit)
    bk 50

drawLabel :: Int -> Float -> TurtleCommand ()
drawLabel i  unit = branch $ do
  setSpeed 0
  setRotationSpeed  0
  setPenUp
  fd 200
  setHeading 90
  fd $ min 0 (unit * 50)
  c <- penColor
  setHeading 0
  setRepresentation $ color c $ scale 0.2 0.2 $ text (labels !! i)
  stamp

-- | Generic `reflect2` which draws some generic argument called drawing.
reflect2 ::Float -> Float -> Int -> TurtleCommand ()
reflect2 mirror1 mirror2 i
  | i >= n*2 = return ()
  | otherwise = do
    
    setPenColor white
    drawFlag 1
    drawLabel i 1 
    
    heading >>= \h -> lt (2 * (mirror1 - h))
    setPenColor red
    drawFlag (-1)
    drawLabel (i + 1) (-1)
    
    heading >>= \h -> lt (2 * (mirror2 - h))
    reflect2 mirror1 mirror2 (i + 2)

main :: IO ()
main = runTurtle' black $ do
    setSpeed 300
    setRotationSpeed 0
    setHeading east -- set to 0 heading.
    reflect2 0 (360/(2* fromIntegral n)) 0