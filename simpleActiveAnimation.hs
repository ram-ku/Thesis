import Active_lib
import Animator
import Graphics.Blank
import qualified Data.Text as Text

box = trimBefore $ mkActive "box" boxCanvas boxBFn 0 3 boxOFn []
line1= trimBefore $ mkActive "line" lineCanvas emptyBFN 0 3 lineOFN []
delay1 = trimBefore $ mkActive "delay1" verticalDelayCanvas delayBFn 0 3 originFn1 [("box",originFn2)]

main :: IO ()
main = blankCanvas 3000 { events = [Text.pack "click"] } $ \ context -> do
							play context "Play" [] $ simulateActive (toRational 30) $ line1 <=> box <=> delay1
					
verticalDelayCanvas (x,y) = rectangle x y 10 40 "black" "black"
boxCanvas (x,y) = rectangle x y 80 40 "red" "black"
lineCanvas (x,y) = line x y 300 "right"

boxBFn (x,y) = [((x,y),(x+80,y)),((x+80,y),(x+80,y+40)),((x+80,y+40),(x,y+40)),((x,y+40),(x,y))]
delayBFn (x,y) = [((x,y),(x+10,y)),((x+10,y),(x+10,y+40)),((x+10,y+40),(x,y+40)),((x,y+40),(x,y))]
emptyBFN _ = []

boxOFn _ _ _= (500,300)
lineOFN _ _ _ = (380,320)

originFn1 :: OriginFn
originFn1 startTime (xco,yco) currTime = (x + (50 *time) , 300)
								where
									x = if xco == -1 then 400 else xco
									time = (fromTime currTime) - (fromTime startTime)

originFn2 :: OriginFn
originFn2 startTime _ currTime = (600 + (time * 50),300)
								where
									time = (fromTime currTime) - (fromTime startTime)