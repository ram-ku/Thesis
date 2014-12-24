import Active_lib
import AlgebraAnimator
import Graphics.Blank
import qualified Data.Text as Text

staticElements = trimBefore $ mkActive "staticElements" staticElementsCanvas emptyBFN 0 22 staticElementsOFN []
delay1 = trimBefore $ mkActive "delay1" verticalDelayCanvas emptyBFN 0 22 delayOFN1 []
delay2 = trimBefore $ mkActive "delay2" verticalDelayCanvas emptyBFN 0 22 delayOFN2 []
delay3 = trim $ mkActive "delay3" horizontalDelayCanvas emptyBFN 1 (8.5) delayOFN3 []
delay4 = trim $ mkActive "delay4" horizontalDelayCanvas emptyBFN 0 7 delayOFN4 []
delay5 = trim $ mkActive "delay5" horizontalDelayCanvas emptyBFN 2 4.25 delayOFN5 []
delay6 = trimBefore $ mkActive "delay6" verticalDelayCanvas emptyBFN 4.25 22 delayOFN6 []
delay7 = trimBefore $ mkActive "delay7" horizontalDelayCanvas emptyBFN 4.95 22 delayOFN7 []
delay8 = trim $ mkActive "delay8" verticalDelayCanvas emptyBFN 3 13 delayOFN8 []
delay9 = trimBefore $ mkActive "delay9" verticalDelayCanvas emptyBFN 0 22 delayOFN9 []
delay10 = trimBefore $ mkActive "delay10" verticalDelayCanvas emptyBFN 9 22 delayOFN10 []
delay11 = trim $ mkActive "delay11" horizontalDelayCanvas emptyBFN 10.25 15 delayOFN11 []
delay12 = trim $ mkActive "delay12" horizontalDelayCanvas emptyBFN 10.75 13 delayOFN12 []
delay13 = trimBefore $ mkActive "delay13" verticalDelayCanvas emptyBFN 16 22 delayOFN13 []
delay14 = trim $ mkActive "delay14" horizontalDelayCanvas emptyBFN 17.5 20 delayOFN14 []
delay15 = trim $ mkActive "delay15" verticalDelayCanvas emptyBFN 21 22 delayOFN15 []
delay16 = trim $ mkActive "delay16" horizontalDelayCanvas emptyBFN 12 22 delayOFN16 []
delay17 = trim $ mkActive "delay17" horizontalDelayCanvas emptyBFN 19 22 delayOFN17 []


main :: IO ()
main = blankCanvas 3000 { events = [Text.pack "click"] } $ \ context -> do
                    play context "Play" [2,3,4] $ simulateActive (toRational 30) $ staticElements <=> delay1 <=> delay2 <=> delay3 <=> delay4 <=> delay5 <=> delay6 <=> delay7 <=> delay8 <=> delay9 <=> delay10 <=> delay11 <=> delay12 <=> delay13 <=> delay14 <=> delay15 <=> delay16 <=> delay17

verticalDelayCanvas (x,y) = rectangle x y 10 40 "black" "black"
horizontalDelayCanvas (x,y) = rectangle x y 40 10 "black" "black"
staticElementsCanvas (x,y) = do
								outerRectangleUp x y 410 300
								line x y 15 "right"
								square (x+15) (y-25) 50 "green"
								line (x+65) y 15 "right"
								square (x+80) (y-25) 50 "green"
								line (x+130) y 40 "right"
								square (x+170) (y-20) 40 "blue"
								line (x+210) y 40 "right"
								rombus (x+265) (y-15) 30 "orange"
								line (x+280) y 10 "right"
								rombus (x+305) (y-15) 30 "orange"
								line (x+320) y 20 "right"
								square (x+340) (y-25) 50 "green"
								line (x+390) y 40 "right"
								rombus (x+445) (y-15) 30 "orange"
								line (x+460) y 20 "right"
								square (x+480) (y-25) 50 "green"
								line (x+530) y 90 "right"
								line (x+40) (y-300) 80 "down"
								line (x+305) (y-15) 45 "up"
								line (x+305) (y-60) 105 "right"
								rectangle (x+15) (y-220) 50 30 "pink" "Black"
								line (x+40) (y-190) 90 "down"
								circle (x+40) (y-95) 5 "white"
								line (x+50) (y-90) 15 "up"
								line (x+50) (y-105) 145 "right"
								gate1 (x+25) (y-90) "pink"
								line (x+40) (y-65) 40 "down"
								rectangle (x+170) (y-220) 45 30 "pink" "black"
								line (x+195) (y-190) 120 "down"
								line (x+150) (y-205) 205 "down"
								line (x+150) (y-205) 20 "right"
								line (x+215) (y-205) 15 "right"
								line (x+230) (y-205) 205 "down"
								line (x+40) (y-115) 150 "right"
								line (x+190) (y-115) 42 "down"								
								gate2 (x+185) (y-70) "pink"
								line (x+195) (y-50) 30 "down"
								line (x+560) y 60 "down"
								line (x+560) (y+60) 455 "left"
								line (x+445) (y+60) 45 "up"
								line (x+265) (y+60) 45 "up"
								line (x+105) (y+60) 35 "up"
								
emptyBFN :: BoundaryFn
emptyBFN _ = []

staticElementsOFN _ _ _ =(400,400)

delayOFN1 _ _ currTime 
					| ct <= 1				= (535,380)
					| ct > 1 && ct <= 2 	= (535 + ((ct - 1) * 20),380)
					| ct > 2 && ct <= 8 	= (555,380)
					| ct > 8 && ct <= 9 	= (555 + ((ct - 8) * 60),380)
					| ct > 9 && ct <= 11 	= (615,380)
					| ct > 11 && ct <= 12 	= (615 + ((ct - 11)* 20),380)
					| ct > 12 && ct <= 13 	= (635 + ((ct - 12)* 90),380)
					| ct > 13 && ct <= 14 	= (725 + ((ct - 13)* 70),380) 
					| ct > 14 && ct <= 18 	= (795,380)
					| ct > 18 && ct <= 19 	= (795 + ((ct - 18)* 20),380)
					| ct > 19 && ct <= 20 	= (815 + ((ct - 19)* 50),380)
					| ct > 20 && ct <= 21 	= (865 + ((ct - 20)* 80),380)
					| ct >21 && ct <=22 	= (945 + ((ct - 21)* 20),380)
					| otherwise 			=  (965,380)
					where ct = fromTime currTime

delayOFN2 _ _ currTime 
				| ct <= 2 				= (795,380)
				| ct > 2 && ct <= 3 	= (795 + ((ct- 2)* 20),380)
				| ct > 3 && ct <= 14 	= (815,380)
				| ct > 14 && ct <= 15 	= (815 + ((ct-14)* 50),380)
				| ct > 15 && ct <= 16 	= (865 + ((ct-15)* 80),380)
				| ct > 16 && ct <= 17 	= (945 + ((ct-16)* 40),380)
				| otherwise 			= (985,380)
				where ct = fromTime currTime
				
delayOFN3 _ _ currTime
				| ct >= 1 && ct <= 2 	=(530,375 - ((ct -1)*40))
				| ct > 2 && ct <= 6		=(530,335)
				| ct > 6 && ct <= 7		=(530+((ct-6)*45),335 -((ct-6)*115))
				| ct > 7 && ct <= 8		=(575,220+((ct-7)*135))
				| ct > 8 && ct <=8.5	=(575,355)
				where ct = fromTime currTime
				
delayOFN4 _ _ currTime
				| ct <= 6 			= (610,295)
				| ct > 6 && ct <= 7	= (610-((ct-6)*35),295 -((ct-6)*75))
				where ct = fromTime currTime
delayOFN5 _ _ currTime
				| ct >= 2 && ct <= 3		= (790,380 -((ct-2)*30))
				| ct > 3 && ct <= 4 	= (790,350 -((ct-3)*125))
				| ct > 4 && ct <= 4.25 	= (790,225 -((ct-4)*500))
				where ct = fromTime currTime
delayOFN6 _ _ currTime
				| ct >= 4.25 && ct <= 5	= (800-((ct-4.25)*500),80)
				| otherwise 			= (420,80)
				where ct = fromTime currTime
delayOFN7 _ _ currTime
				| ct >= 4.95 && ct <= 5	= (420,140)
				| ct > 5 && ct <= 6		= (420,140+((ct-5)*100))
				| ct > 6 && ct <= 7		= (420,240)
				| ct > 7 && ct <= 8		= (420,240+((ct-7)*105))
				| otherwise 			= (420,345)
				where ct = fromTime currTime
				
delayOFN8 _ _ currTime 
				| ct >= 3 && ct <= 4 	= (800-((ct-3)*75),320)
				| ct > 4 && ct <= 12 	= (725,320)
				| ct > 12 && ct <= 13	= (725,320+((ct-12)*60))
				where ct = fromTime currTime
				
delayOFN9 _ _ currTime
				| ct <= 9 =(945,380)
				| ct > 9 && ct <= 10	= (945+((ct-9)* 60),380)
				| otherwise				= (1005,380)
				where ct = fromTime currTime
				
delayOFN10 _ _ currTime
				| ct >= 9 && ct <= 10	= (945-((ct-9)* 45),380+((ct-9)* 60))
				| ct > 10 && ct <= 11	= (900-((ct-10)* 380),440)
				| otherwise				= (520,440)
				where ct = fromTime currTime
				
delayOFN11 _ _ currTime
				| ct >= 10.25 && ct <=15 = (825,440)
				where ct = fromTime currTime
delayOFN12 _ _ currTime
				| ct >= 10.75 && ct <= 13 = (645,440)
				where ct = fromTime currTime
				
delayOFN13 _ _ currTime
				| ct >= 16 && ct <= 17	= (945-((ct-16)* 45),380+((ct-16)* 60))
				| ct > 17 && ct <= 18	= (900-((ct-17)* 140),440)
				| otherwise				= (760,440)
				where ct = fromTime currTime
				
delayOFN14 _ _ currTime
				| ct >= 17.5 && ct <=20 = (825,440)
				where ct = fromTime currTime
				
delayOFN15 _ _ currTime
				| ct >= 21 && ct <= 22	= (945-((ct-21)* 45),380+((ct-21)* 60))
				| otherwise				= (900,440)
				where ct = fromTime currTime
				
delayOFN16 _ _ currTime
				| ct >= 12 && ct <=22 = (610,295)
				where ct = fromTime currTime
				
delayOFN17 _ _ currTime
				| ct >= 19 && ct <=22 = (790,350)
				where ct = fromTime currTime
				