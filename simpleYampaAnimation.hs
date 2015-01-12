module AnimateBox where

import Box
import BoxAnimator

import Graphics.Blank

-- set browser to: http://localhost:3000/

main :: IO ()
main = blankCanvas 3000 $ \context -> do runSFcanvas (return ()) (bouncingBox box1) renderBall context

--renderBall :: (Double,Double) -> Box -> Canvas Text
--renderBall  size [] = do return ()
renderBall  size b = do 
					x <- scaleX size (boxPosX b)
					y <- scaleY size (boxPosY b)
					w <- scaleLength size (boxWidth b)
					h <- scaleLength size (boxHeight b)
					x1 <- scaleX size 0.6
					y1 <- scaleY size 0.5
					w1 <- scaleLength size 0.05
					h1 <- scaleLength size 0.06
					x2 <- scaleX size 0.45
					y2 <- scaleY size 0.46
					col <- return (boxCol b)
					line x2 y2 400 "right"
					rectangle x y w h col
					rectangle x1 y1 w1 h1 "red"