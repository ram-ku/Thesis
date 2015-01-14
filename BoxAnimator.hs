module BoxAnimator where

import FRP.Yampa

import Data.Time.Clock
import Data.IORef
import Data.Text

import Graphics.Blank hiding (scale)

type Height = Double
type Width = Double
type Radius = Double
type XCo = Double
type YCo = Double
type Colour = String
type Scale = Double
type Direction = String

circle :: XCo -> YCo -> Radius -> Colour -> Canvas ()
circle x y r col = do beginPath ()
                      arc (x,y,r,0,pi*2,False)
                      closePath ()
                      fillStyle $ pack col
                      fill ()

-- If you've got the very latest version of blank-canvas, then this can replace the "rectangle" definition.
--
-- rectangle :: XCo -> YCo -> Width -> Height -> Colour -> Canvas ()
-- rectangle x y w h col = do fillStyle col
--                            fillRect (x,y,w,h)

rectangle :: XCo -> YCo -> Width -> Height -> Colour -> Canvas ()
rectangle x y w h col = do beginPath ()
                           moveTo (x,y)
                           lineTo (x+w,y)
                           lineTo (x+w,y+h)
                           lineTo (x,y+h)
                           lineTo (x,y)
                           closePath ()
                           fillStyle $ pack col
                           fill ()
						   
line :: XCo -> YCo -> Double -> Direction -> Canvas ()
line x y l dir = do 
					moveTo (x,y)
					case dir of
						"left" -> lineTo (x-l,y)
						"right" -> lineTo (x+l,y)
						"up" -> lineTo (x,y-l)
						"down" -> lineTo (x,y+l)
					strokeStyle $ pack "black"
					stroke ()

scale :: Scale -> Double -> Double
scale s f = realToFrac s * f

scaleLength :: (Double,Double) -> Scale -> Canvas Double
scaleLength (x,y) s = return (scale s ((x+y)/2))

scaleX :: (Double,Double) -> Scale -> Canvas XCo
scaleX (x,_) s = return (scale s x)

scaleY :: (Double,Double) -> Scale -> Canvas YCo
scaleY (_,y) s = return (scale (1-s) y)

runSFcanvas :: IO a -> SF a b -> ((Double,Double) -> b -> Canvas ()) -> DeviceContext -> IO ()
runSFcanvas inp sf r canvas = do 
								 let size = (width canvas,height canvas)
								 t0  <- getCurrentTime
								 ref <- newIORef t0
								 reactimate inp
											(\_ -> do 
													  dt <- getTick ref
													  a  <- inp
													  return (dt,Just a))
											(\ _ b -> do {send canvas (render size (r size b));return False})
											sf

getTick :: IORef UTCTime -> IO DTime
getTick x = do t0 <- readIORef x
               t1 <- getCurrentTime
               writeIORef x t1
               return (realToFrac (diffUTCTime t1 t0))

render :: (Double,Double) -> Canvas a -> Canvas a
render (width,height) r = do
				clearRect (0,0,width,height)
				fillStyle $ pack "white"
				fillRect (0,0,width,height)
				beginPath ()
				save ()
				r
