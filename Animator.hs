module Animator where

import Graphics.Blank hiding (scale)

type Height = Float
type Width = Float
type Radius = Float
type XCo = Float
type YCo = Float
type Colour = String
type StrokeStyle = String
type Direction = String


circle :: XCo -> YCo -> Radius -> Colour -> Canvas ()
circle x y r col = do beginPath ()
                      arc (x,y,r,0,pi*2,False)
                      closePath ()
                      fillStyle col
                      fill ()

rectangle :: XCo -> YCo -> Width -> Height -> Colour -> StrokeStyle -> Canvas ()
rectangle x y w h col strk= do {beginPath ();
                           moveTo (x,y);
                           lineTo (x+w,y);
                           lineTo (x+w,y+h);
                           lineTo (x,y+h);
                           lineTo (x,y);
                           closePath ();
						   strokeStyle strk;
						   stroke ();
						   fillStyle col;
                           fill ()}
						   
square :: XCo -> YCo -> Width -> Colour -> Canvas ()
square x y w col = do {beginPath ();
                       moveTo (x,y);
                       lineTo (x+w,y);
                       lineTo (x+w,y+w);
                       lineTo (x,y+w);
                       lineTo (x,y);
                       closePath ();
					   strokeStyle "black";
					   stroke ();
                       fillStyle col;
                       fill ()}

triangle :: XCo -> YCo ->Direction -> Colour -> StrokeStyle -> Canvas ()
triangle x y dir col strk = do {beginPath ();
					moveTo (x,y);
					(case dir of
						"down" -> do{lineTo (x,y+100); lineTo (x+150,y+100)}
						"right" -> do{lineTo (x+100,y); lineTo (x+100,y-150)}
						"up" -> do{lineTo (x,y-100); lineTo (x-150,y-100)}
						"left" -> do{lineTo (x-100,y);lineTo (x-100,y+150)});
                    lineTo (x,y);
                    closePath ();
					strokeStyle strk;
					stroke ();
                    fillStyle col;
                    fill ()}

line2 :: XCo -> YCo -> StrokeStyle -> Canvas ()
line2 x y strk = do
					beginPath()
					moveTo (x,y)
					lineTo ((x-550),y)
					lineTo ((x-550),(y-300))
					strokeStyle strk
					stroke ()
						
render :: Canvas a -> Canvas ()
render r = do (width,height) <- size
              clearRect (0,0,width,height)
              beginPath ()
              save ()
              r
              restore ()
