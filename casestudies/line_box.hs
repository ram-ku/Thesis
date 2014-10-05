{-# LANGUAGE FlexibleInstances,InstanceSigs,UndecidableInstances#-}
module Active_Canvas_Monoid where

import Graphics.Blank
import Animator
import Data.List
import Data.Active
import Data.Semigroup
import Control.Monad

-- makes Canvas an instance of semigroup 
instance Semigroup (Canvas ()) where
	(<>) = mappend

-- makes Canvas an instance of Monoid
instance Monoid a => Monoid (Canvas a) where 
	mempty = return mempty
	mappend = liftM2 mappend

-- combines many Actives into one.This elongates the length of the animation
join_actives :: [Active (Canvas ())] -> Active (Canvas ())
join_actives xs = foldl1 (->>) xs

--Converts from time to float					
fromTimeF::Time -> Float
fromTimeF=fromTime

--here we make different parts of the animation. Each acts differently based on time which will later be comibined into to one Big active
lineActive = mkActive (toTime 0) (toTime 0) (\x ->if (fromTimeF x) >= 0 then renderLine else return ())
movingBox = mkActive (toTime 0) (toTime 20) (\x -> if (fromTimeF x) >=0 && (fromTimeF x) <=10 
													then rectangle (700 - (60 * fromTimeF x)) 400 100 100 "Yellow" "Black"
													else rectangle 100 (400 -(30 * ((fromTimeF x)-10))) 100 100 "Yellow" "Black")

-- main function which starts the animation
main :: IO ()
main = blankCanvas 3000 $ \ context -> play context $ simulate (toRational 30) $ join_actives [lineActive,movingBox]

play context (n:ns) = do{
				send context $ do {
					render n};
				play context ns}
play context [] = do { send context $ do {fillText ("The End",610,250)}}

renderLine ::Canvas ()
renderLine = line2 700 450 "Black" 