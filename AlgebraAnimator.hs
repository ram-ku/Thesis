module AlgebraAnimator where
import Graphics.Blank hiding (scale)
import Data.Text

type Height = Double
type Width = Double
type Radius = Double
type XCo = Double
type YCo = Double
type Color = String
type StrokeStyle = String
type Direction = String

circle :: XCo -> YCo -> Radius -> Color -> Canvas ()
circle x y r col = do 
					arc (x,y,r,0,pi*2,False)
					strokeStyle $ pack "black"
					stroke ()

rombus ::XCo -> YCo -> Width ->Color -> Canvas ()
rombus x y w col= do
				beginPath ()
				moveTo (x,y)
				lineTo (x + (w/2), y+ (w/2))
				lineTo (x , y+w)
				lineTo (x - (w/2),y + (w/2))
				closePath ()
				strokeStyle $ pack "black"
				stroke ()
				fillStyle $ pack col
				fill ()
				
rectangle :: XCo -> YCo -> Width -> Height -> Color -> StrokeStyle -> Canvas ()
rectangle x y w h col strk= do {beginPath ();
                           moveTo (x,y);
                           lineTo (x+w,y);
                           lineTo (x+w,y+h);
                           lineTo (x,y+h);
                           lineTo (x,y);
                           closePath ();
						   strokeStyle $ pack strk;
						   stroke ();
						   fillStyle $ pack col;
                           fill ()}				


square :: XCo -> YCo -> Width -> Color -> Canvas ()
square x y w col = do {beginPath ();
                       moveTo (x,y);
                       lineTo (x+w,y);
                       lineTo (x+w,y+w);
                       lineTo (x,y+w);
                       lineTo (x,y);
                       closePath ();
					   strokeStyle $ pack "black";
					   stroke ();
                       fillStyle $ pack col;
                       fill ()
					   }

emptySquare :: XCo -> YCo -> Width -> Canvas ()
emptySquare x y w = do {beginPath ();
                       moveTo (x,y);
                       lineTo (x+w,y);
                       lineTo (x+w,y+w);
                       lineTo (x,y+w);
                       lineTo (x,y);
                       closePath ();
					   strokeStyle $ pack "black";
					   stroke ();}
					   
outerRectangle :: XCo -> YCo -> Width -> Height -> Canvas ()
outerRectangle x y w h = do {moveTo (x,y+h);
                           lineTo (x,y);
                           lineTo (x+w,y);
                           lineTo (x+w,y+h);
						   strokeStyle $ pack "black";
						   stroke ();}
						   
outerRectangleUp :: XCo -> YCo -> Width -> Height -> Canvas ()
outerRectangleUp x y w h = do {moveTo (x,y);
                           lineTo (x,y-h);
                           lineTo (x+w,y-h);
                           lineTo (x+w,y);
						   strokeStyle $ pack "black";
						   stroke ();}
						   
gate1 :: XCo -> YCo -> Color -> Canvas()
gate1 x y col= do
				beginPath ()
				moveTo (x,y)
				lineTo (x+30,y)
				lineTo (x+30,y+20)
				lineTo (x+15,y+25)
				lineTo (x,y+20)
				closePath ()
				strokeStyle $ pack "black"
				stroke ()
				fillStyle $ pack col
				fill ()
				
gate2 :: XCo -> YCo -> Color -> Canvas()
gate2 x yco col= do
				beginPath ()
				moveTo (x,y)
				lineTo (x+10,y+5)
				lineTo (x+20,y)
				lineTo (x+20,y+20)
				lineTo (x+10,y+25)
				lineTo (x,y+20)
				closePath ()
				strokeStyle $ pack "black"
				stroke ()
				fillStyle $ pack col
				fill ()
			where
				y = yco - 5
				
-- gate2 :: XCo -> YCo -> Color->Canvas()
-- gate2 x y col= do
				-- beginPath()
				-- moveTo (x,y)
				-- arc (x+10,y-10,14.14,0.25*pi,0.75*pi,False)
				-- lineTo (x,y+20)
				-- arc (x+10,y+10,14.14,0.25*pi,0.75*pi,False)
				-- --moveTo (x+20,y+20)
				-- lineTo (x+20,y)
				-- --closePath()
				-- strokeStyle $ pack "black"
				-- stroke ()
				-- fillStyle $ pack col
				-- fill ()
				

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