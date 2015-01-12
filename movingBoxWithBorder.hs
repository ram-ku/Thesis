import Active_lib
import Animator
import Graphics.Blank
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

testDynamic1 = mkDynamic "testDynamic1" square1 square1BFn 0 10 originFn1 [("testDynamic2",originFn2)]
testDynamic2 = mkDynamic "testDynamic2" square2 square1BFn 0 10 originFn3 [("testDynamic1",originFn4)]

squareActive1 = trimBefore $ mkActive "squareActive1" square1 square1BFn 0 20 originFn1 [("squareActive2",originFn2),("borderLeft",originFn1),("borderRight",originFn2)]
squareActive2 = trimBefore $ mkActive "squareActive2" square2 square1BFn 0 10 originFn3 [("squareActive1",originFn4),("borderLeft",originFn4),("borderRight",originFn3)]
borderTop = trimBefore $ mkActive "borderTop" lineTop lineTopBFn 0 10 lineTopOFN []
borderRight = trimBefore $ mkActive "borderRight" lineRight lineRightBFn 0 10 lineRightOFN []
borderLeft = trimBefore $ mkActive "borderLeft" lineLeft lineLeftBFn 0 10 lineLeftOFN []
borderBottom = trimBefore $ mkActive "borderBottom" lineBottom lineBottomBFn 0 10 lineBottomOFN []

originFn1 :: Time -> Point -> Time -> Origin
originFn1 startTime (xco,yco) currTime = (x + (50 *time) , 300)
								where
									x = if xco == -1 then 400 else xco
									time = (fromTime currTime) - (fromTime startTime)

originFn2 :: Time -> Point -> Time -> Origin
originFn2 startTime (x,y) currTime = (x - (time * 50),300)
								where
									time = (fromTime currTime) - (fromTime startTime)

originFn3 :: Time -> Point -> Time -> Origin
originFn3 startTime (xco,yco) currTime =(x - (50 * time) , 300)
								where
									x = if xco == -1 then 900 else xco
									time = (fromTime currTime) - (fromTime startTime)

originFn4:: Time -> Point -> Time -> Origin
originFn4 startTime (x,y) currTime = (x + (time * 50), 300)
								where
									time = (fromTime currTime) - (fromTime startTime)
									
originFn5 :: Time -> Point -> Time -> Origin
originFn5 startTime startPosition currTime =((900 - ( 50 *(fromTime currTime))) , 400)

square1BFn :: Origin -> [Line]
square1BFn (x,y) = [((x,y),(x+100,y)),((x+100,y),(x+100,y+100)),((x+100,y+100),(x,y+100)),((x,y+100),(x,y))]

square1 :: Origin -> Canvas ()
square1 (x,y) = if x >= 0 && y >= 0 then square x y 100 "Red" else return()

square2 :: Origin -> Canvas ()
square2 (x,y) = if x >= 0 && y >= 0 then square x y 100 "Blue" else return ()

lineTopOFN _ _ _	=(0,0)
lineRightOFN _ _ _ 	=(1300,0)
lineLeftOFN _ _ _	=(0,0)
lineBottomOFN _ _ _ =(0,600)
lineTopBFn _ =[((0,0),(1300,0))]
lineRightBFn _ =[((1300,0),(1300,600))]
lineLeftBFn _ =[((0,0),(0,600))]
lineBottomBFn _ =[((0,600),(1300,600))]
lineTop :: Origin -> Canvas ()
lineTop (x,y) = line x y 1300 "right"

lineRight :: Origin -> Canvas ()
lineRight (x,y) = line x y 600 "down"

lineLeft :: Origin -> Canvas ()
lineLeft (x,y) = line x y 600 "down"

lineBottom :: Origin -> Canvas ()
lineBottom (x,y) = line x y 1300 "right"


-- main function which starts the animation
main :: IO ()
main = blankCanvas 3000 { events = [Text.pack "click"] } $ \ context -> do
					play context "Play" [2,3] $ simulateActive (toRational 30) $ borderTop <=> borderRight <=> borderLeft <=> borderBottom <=> squareActive1 <=> squareActive2