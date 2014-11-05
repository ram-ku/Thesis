{-# LANGUAGE DeriveFunctor
           , GeneralizedNewtypeDeriving
           , TypeSynonymInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , FlexibleInstances
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.List
import Graphics.Blank
import Animator
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Newtype
import Control.Monad

import Data.Array
import Data.Maybe

import Data.Functor.Apply
import Data.Semigroup hiding (First(..))
import Data.Monoid (First(..))

import Data.VectorSpace hiding ((<.>))
import Data.AffineSpace
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
------------------------------------------------------------
-- Time
------------------------------------------------------------

-- | An abstract type for representing /points in time/.  Note that
--   literal numeric values may be used as @Time@s, thanks to the the
--   'Num' and 'Fractional' instances.  'toTime' and 'fromTime' are
--   also provided for convenience in converting between @Time@ and
--   other numeric types.
newtype Time = Time { unTime :: Rational }
  deriving ( Eq, Ord, Show, Read, Enum, Num, Fractional, Real, RealFrac
           , AdditiveGroup, InnerSpace
           )

instance Newtype Time Rational where
  pack   = Time
  unpack = unTime

instance VectorSpace Time where
  type Scalar Time = Rational
  s *^ (Time t) = Time (s * t)

-- | Convert any value of a 'Real' type (including @Int@, @Integer@,
--   @Rational@, @Float@, and @Double@) to a 'Time'.
toTime :: Real a => a -> Time
toTime = fromRational . toRational

-- | Convert a 'Time' to a value of any 'Fractional' type (such as
--   @Rational@, @Float@, or @Double@).
fromTime :: Fractional a => Time -> a
fromTime = fromRational . unTime

-- | An abstract type representing /elapsed time/ between two points
--   in time.  Note that durations can be negative. Literal numeric
--   values may be used as @Duration@s thanks to the 'Num' and
--   'Fractional' instances. 'toDuration' and 'fromDuration' are also
--   provided for convenience in converting between @Duration@s and
--   other numeric types.
newtype Duration = Duration { unDuration :: Rational }
  deriving ( Eq, Ord, Show, Read, Enum, Num, Fractional, Real, RealFrac
           , AdditiveGroup)

instance Newtype Duration Rational where
  pack   = Duration
  unpack = unDuration

instance VectorSpace Duration where
  type Scalar Duration = Rational
  s *^ (Duration d) = Duration (s * d)

instance AffineSpace Time where
  type Diff Time = Duration
  (Time t1) .-. (Time t2) = Duration (t1 - t2)
  (Time t) .+^ (Duration d) = Time (t + d)

-- | Convert any value of a 'Real' type (including @Int@, @Integer@,
--   @Rational@, @Float@, and @Double@) to a 'Duration'.
toDuration :: Real a => a -> Duration
toDuration = fromRational . toRational

-- | Convert a 'Duration' to any other 'Fractional' type (such as
--   @Rational@, @Float@, or @Double@).
fromDuration :: Fractional a => Duration -> a
fromDuration = fromRational . unDuration

-- | An @Era@ is a concrete span of time, that is, a pair of times
--   representing the start and end of the era. @Era@s form a
--   semigroup: the combination of two @Era@s is the smallest @Era@
--   which contains both.  They do not form a 'Monoid', since there is
--   no @Era@ which acts as the identity with respect to this
--   combining operation.
--
--   @Era@ is abstract. To construct @Era@ values, use 'mkEra'; to
--   deconstruct, use 'start' and 'end'.
newtype Era = Era (Min Time, Max Time)
  deriving (Semigroup, Show)

-- | Create an 'Era' by specifying start and end 'Time's.
mkEra :: Time -> Time -> Era
mkEra s e = Era (Min s, Max e)

-- | Get the start 'Time' of an 'Era'.
start :: Era -> Time
start (Era (Min t, _)) = t

-- | Get the end 'Time' of an 'Era'.
end :: Era -> Time
end (Era (_, Max t)) = t

-- | Compute the 'Duration' of an 'Era'.
duration :: Era -> Duration
duration = (.-.) <$> end <*> start

type Name=String
type X_cord=Double
type Y_cord=Double
type Low_y= Y_cord
type High_y=Y_cord
type Point=(X_cord,Y_cord)
type Origin = Point
type OriginFn = Time -> Time -> Origin
type CanvasFn a = Origin -> a
type Line=(Point,Point)
type Shape=(Name,[Line])
type ShapeMap=(Name,Map.Map Y_cord (X_cord,X_cord))
type BoundaryFn = Origin -> [Line]
type Eqn = Y_cord -> (X_cord,X_cord)
type ActEvent = Name
type Delay =Duration
	
									  
mkEquation1 :: Point -> Point -> Eqn
mkEquation1  (x1,y1) (x2,y2) = case slope of
									0 ->(\y -> if y==y1 then(x1,x2) else (-1,-1))
									8->(\y -> if (min y1 y2)<=y && y<=(max y1 y2) then(x1,x2) else (-1,-1))
									_ ->(\y ->let xcord=(((-1*y)+y1)+ slope*x1)/ slope in if (min x1 x2)<=xcord && xcord<=(max x1 x2) then(xcord,xcord) else (-1,-1))
									where slope = if (y1==y2) then 0 --horizontal line
												else if (x1==x2) then 8 --vertical line -- 8 here means infinity
												else ((-1*y2)+y1)/(x2-x1) --sloping line

mkEquations :: [(Point,Point)] -> [Eqn]
mkEquations [] = []
mkEquations ((point1,point2):points) = (mkEquation1 point1 point2 : mkEquations points)


eqnsToMap :: [Eqn] -> Low_y -> High_y -> Map.Map Y_cord (X_cord,X_cord)
eqnsToMap eqns l h = convertAllEqnsIntoMap eqns Map.empty l h
			where
				convertAllEqnsIntoMap eqs m l h = foldl' (convertOneEqnIntoMap l h) m eqs
				convertOneEqnIntoMap l h m eq = foldl' insertIntoMap m $ getAllPointFromEqn eq l h
				insertIntoMap m (y,(x1,x2))=Map.insertWith findMinMax y (x1,x2) m
				findMinMax (x1,x2) (x3,x4) = (if x1 < x3 then x1 else x3 , if x2 > x4 then x2 else x4)
				getPointUsingY eq y = (y,eq y)
				getAllPointFromEqn eq l h = filter validPoints $ map (getPointUsingY eq) [l,l+1..h]
				validPoints (y,(x1,x2)) = if x1== -1 then False else True

shapesToShapeMaps :: [Shape] -> [ShapeMap]
shapesToShapeMaps shapes = map (\ (name,lines) -> (name, linesToMap lines)) shapes

linesToMap::[Line] -> Map.Map Y_cord (X_cord,X_cord)
linesToMap [] = Map.empty
linesToMap lines = eqnsToMap (mkEquations lines) (findMinY lines) (findMaxY lines)
					where
						findMinY lines = findMinY_aux lines 9999999
						findMaxY lines = findMaxY_aux lines (-1)
						findMinY_aux [] min = min
						findMinY_aux ((p1,p2):lines) min = findMinY_aux lines (find_min p1 p2 min)						
						findMaxY_aux [] max = max
						findMaxY_aux ((p1,p2):lines) max = findMaxY_aux lines (find_max p1 p2 max)
						find_max = (\(x1,y1) (x2,y2) m -> if y1 >= y2 then (if y1 > m then y1 else m ) else (if y2 > m then y2 else m))
						find_min = (\(x1,y1) (x2,y2) m -> if y1 <= y2 then (if y1 < m then y1 else m ) else (if y2 < m then y2 else m))

																			 
traverseShapesAndDetect :: [ShapeMap] -> Map.Map Name [ActEvent] -> Map.Map Name [ActEvent]
traverseShapesAndDetect [] a = a
traverseShapesAndDetect (shapeM:shapesM) a = traverseShapesAndDetect shapesM $ detect shapeM shapesM a

detect :: ShapeMap -> [ShapeMap] -> Map.Map Name [ActEvent] -> Map.Map Name [ActEvent]
detect shapeM shapesM a = foldl (detect_aux shapeM) a shapesM
							where detect_aux = (\(n1,m1) a (n2,m2) -> if m1 /= Map.empty && m2 /= Map.empty && (compareMaps m1 m2)
																	  then Map.insertWith (++) n1 [n2] $ Map.insertWith (++) n2 [n1] a
																	  else a )

compareMaps :: Map.Map Y_cord (X_cord,X_cord) -> Map.Map Y_cord (X_cord,X_cord) -> Bool
compareMaps m1 m2 = if m1min > m2max || m2min > m1max 
						then False
						else foldl (\a k -> if a then a else check (Map.lookup k m1) (Map.lookup k m2)) False $ Map.keys m1
					where 
						(m1min,_) = Map.findMin m1
						(m1max,_) = Map.findMax m1
						(m2min,_) = Map.findMin m2
						(m2max,_) = Map.findMax m2
						check _ Nothing = False
						check (Just (x1,x2)) (Just (x3,x4)) = if x1>x4 || x3>x2 then False else True

-- makes Canvas an instance of semigroup 
instance Semigroup (Canvas ()) where
	(<>) = mappend

data Evnt a = Evnt a
	deriving (Eq, Ord, Show)
	
data Dynamic a = Dynamic { era        		:: Era
						 , originMap 		:: Map.Map Name (Time,OriginFn)
						 , eventOriginMap 	:: Map.Map Name [(ActEvent,OriginFn)]
                         , runDynamic 		:: Time -> Dynamic a -> a
						 , boundaryMap 		:: Map.Map Name (BoundaryFn,Delay)
                         }
						 
getShapes :: Dynamic a -> Time -> [Shape]
getShapes d t = map (\(n,(bfn,del)) -> if fromTime (t .-^ del) >= 0 then (n, bfn ((\(Just (st,ofn))-> ofn st (fromTime (t .-^ del))) (Map.lookup n (originMap d)))) else (n,[]))  $ Map.toList $ boundaryMap d

--changeDynamic d t eventMap = d {originMap = Map.fromList [("newDynamic1", ( t ,originFn2)),("newDynamic2",(t,originFn3))]}
changeDynamic d t oldEventMap eventMap = if (Map.null (eventOriginMap d)) || (Map.null eventMap) 
											then d 
											else foldl (updateFn t) d $ Map.toList $ diffEventMaps oldEventMap eventMap

diffEventMaps :: Map.Map Name [ActEvent] -> Map.Map Name [ActEvent] -> Map.Map Name [ActEvent]
diffEventMaps oldEventMap newEventMap = Map.mapWithKey fn newEventMap
											where
												fn k v = let oldVal= Map.lookup k oldEventMap in
															if oldVal== Nothing
															then v
															else v \\ (fromJust oldVal)


updateFn :: Time -> Dynamic a -> (Name ,[ActEvent]) -> Dynamic a
updateFn t d (n,evntlist)= if (null oFns) 
							then d 
							else if (length oFns) == 0 then d else modifyOriginMap d n (t, (head oFns))
								where
									oFns = getOFNs evntlist $ Map.lookup n (eventOriginMap d)
									getOFNs _ Nothing = []
									getOFNs [] _ = []
									getOFNs evntList (Just evntOriginList) = map snd.head $ map ( \eventName -> filter ( \ (n,ofn) -> n == eventName) evntOriginList ) evntList 									

modifyOriginMap :: Dynamic a -> Name -> ( Time, OriginFn) -> Dynamic a
modifyOriginMap d name timeOFNTuple = d {originMap =Map.insert name timeOFNTuple (originMap d)}

simulate rate d = simulate_aux rate d (start (era d)) (end (era d)) Map.empty

simulate_aux rate d t e oldEventMap = if t > e then [] else currentCanvas: simulate_aux rate changedDynamic (t + (1^/rate)) e newEventMap
							where
								currentCanvas = (runDynamic d) t d
								newEventMap = traverseShapesAndDetect (shapesToShapeMaps (getShapes d t)) Map.empty
								changedDynamic = changeDynamic d t oldEventMap newEventMap


newDynamic1 :: Dynamic (Canvas ())
newDynamic1 = Dynamic { era = mkEra (toTime 0) (toTime 10) <> mkEra (toTime 0) (toTime 5)
						, originMap = Map.fromList [("newDynamic1", ((toTime 0),originFn1))
													,("newDynamic2",((toTime 0),originFn3))
													,("newDynamic3",((toTime 0),originFn5))]
						, eventOriginMap = Map.fromList [("newDynamic1",[("newDynamic2",originFn2)])]
						, runDynamic =	((\t d -> square1 $ (\(Just (st,ofn)) -> ofn st t) $ (Map.lookup "newDynamic1" (originMap d))) <> (\t d -> square2 $ (\(Just (st,ofn)) -> ofn st (t .-^ (toDuration 5))) $ (Map.lookup "newDynamic2" (originMap d)))) <> (\t d -> square1 $ (\(Just (st,ofn)) -> ofn st t) $ (Map.lookup "newDynamic3" (originMap d)))
						, boundaryMap = Map.fromList [("newDynamic1",(square1BFn,toDuration 0)),("newDynamic2",(square1BFn,toDuration 5))]
						}

						
-- main function which starts the animation
main :: IO ()
main = blankCanvas 3000 { events = [Text.pack "click"] } $ \ context -> do
                    e <- atomically $ (fmap Just $ readTChan (eventQueue context)) `orElse` return Nothing
                    putStrLn $ show $ e
                    play context $ simulate (toRational 30) newDynamic1

play context [] = do { send context $ do {fillText ((Text.pack "The End"),650,250)}}
play context (n:ns) = do{
				send context $ do {
					render n (width context) (height context)};
				play context ns}


originFn1 :: Time -> Time -> Origin
originFn1 startTime currTime = if currTime >= 0 then ((400 + ( 50 *(fromTime currTime))) , 300) else (-1,-1)

originFn2 :: Time -> Time -> Origin
originFn2 startTime currTime = if currTime >= 0 
								then(((400+(50 * (fromTime startTime)))  - (((fromTime currTime) - (fromTime startTime))* 50)),300) 
								else (-1,-1)

originFn3 :: Time -> Time -> Origin
originFn3 startTime currTime =if currTime >= 0 then((900 - ( 50 *(fromTime currTime))) , 300) else (-1,-1)

originFn4:: Time -> Time -> Origin
originFn4 startTime currTime = if currTime >= 0 then (startXPosition + (time * 50), 300) else (-1,-1)
								where
									startXPosition= (900 - (50 * (fromTime startTime)))
									time = (fromTime currTime) - (fromTime startTime)
originFn5 :: Time -> Time -> Origin
originFn5 startTime currTime =if currTime >= 0 then((900 - ( 50 *(fromTime currTime))) , 400) else (-1,-1)

square1BFn :: Origin -> [Line]
square1BFn (x,y) = [((x,y),(x+100,y)),((x+100,y),(x+100,y+100)),((x+100,y+100),(x,y+100)),((x,y+100),(x,y))]

square1 :: Origin -> Canvas ()
square1 (x,y) = if x >= 0 && y >= 0 then square x y 100 "Red" else return()

square2 :: Origin -> Canvas ()
square2 (x,y) = if x >= 0 && y >= 0 then square x y 100 "Blue" else return ()

