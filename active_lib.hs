{-# LANGUAGE DeriveFunctor
           , GeneralizedNewtypeDeriving
           , TypeSynonymInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , FlexibleInstances
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import qualified Data.Map.Strict as Map
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
type X_cord=Float
type Y_cord=Float
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
	
mk_equation :: Point -> Point -> Y_cord -> (X_cord,X_cord,Bool)
mk_equation  (x1,y1) (x2,y2) = case m of
								0 ->(\y -> (x1,x2,y==y1))
								8->(\y -> (x1,x2,(min y1 y2)<=y && y<=(max y1 y2)))
								_ ->(\y ->let xcord=((y-y1)- m*x1)/ m in (xcord,xcord, (min x1 x2)<=xcord && xcord<=(max x1 x2)))
								where m = if (y1==y2) then 0 --horizontal line
										  else if (x1==x2) then 8 --vertical line
										  else (y2-y1)/(x2-x1) --sloping line
										  
mk_equation1 :: Point -> Point -> Eqn
mk_equation1  (x1,y1) (x2,y2) = case m of
									0 ->(\y -> if y==y1 then(x1,x2) else (-1,-1))
									8->(\y -> if (min y1 y2)<=y && y<=(max y1 y2) then(x1,x2) else (-1,-1))
									_ ->(\y ->let xcord=(((-1*y)+y1)+ m*x1)/ m in if (min x1 x2)<=xcord && xcord<=(max x1 x2) then(xcord,xcord) else (-1,-1))
									where m = if (y1==y2) then 0 --horizontal line
										  else if (x1==x2) then 8 --vertical line
										  else ((-1*y2)+y1)/(x2-x1) --sloping line

mk_equations :: [(Point,Point)] -> [Eqn]
mk_equations [] = []
mk_equations ((point1,point2):points) = (mk_equation1 point1 point2 : mk_equations points)
--gatherPoints:: [(Y_cord -> (X_cord,X_cord,Bool)] -> Y_cord -> Y_cord ->

get_cord_from_one_y ::  Eqn -> Y_cord -> (Y_cord,(X_cord,X_cord))
get_cord_from_one_y eqn y = (y,eqn y)

get_all_cords_from_eqn :: Eqn -> Low_y -> High_y ->[(Y_cord,(X_cord,X_cord))]
get_all_cords_from_eqn eqn low_y high_y = filter validPoints $ map (get_cord_from_one_y eqn) [low_y,low_y+1..high_y]

validPoints :: (Y_cord,(X_cord,X_cord)) -> Bool
validPoints (y,(x1,x2)) = if x1== -1 then False else True

eqns_to_map :: [Eqn] -> Low_y -> High_y -> Map.Map Y_cord (X_cord,X_cord)
eqns_to_map eqns low_y high_y = cords_from_eqns_to_map eqns Map.empty low_y high_y 

cords_from_eqns_to_map :: [Eqn] -> Map.Map Y_cord (X_cord,X_cord) -> Low_y -> High_y -> Map.Map Y_cord (X_cord,X_cord)
cords_from_eqns_to_map eqns cord_map low_y high_y = foldl (cords_from_eqn_to_map low_y high_y) cord_map eqns

cords_from_eqn_to_map :: Low_y -> High_y -> Map.Map Y_cord (X_cord,X_cord) -> Eqn -> Map.Map Y_cord (X_cord,X_cord)
cords_from_eqn_to_map low_y high_y cord_map eqn = foldl insert_into_map cord_map $ get_all_cords_from_eqn eqn low_y high_y

insert_into_map :: Map.Map Y_cord (X_cord,X_cord) -> (Y_cord,(X_cord,X_cord)) -> Map.Map Y_cord (X_cord,X_cord)
insert_into_map cord_map (y,(x1,x2))=Map.insertWith find_min_max y (x1,x2) cord_map

find_min_max :: (X_cord,X_cord) -> (X_cord,X_cord) -> (X_cord,X_cord)
find_min_max (x1,x2) (x3,x4) = (if x1 < x3 then x1 else x3 , if x2 > x4 then x2 else x4)

shapesToShapeMaps :: [Shape] -> [ShapeMap]
shapesToShapeMaps shapes = map (\ (name,lines) -> (name, linesToMap lines)) shapes

linesToMap::[Line] -> Map.Map Y_cord (X_cord,X_cord)
linesToMap lines = eqns_to_map (mk_equations lines) (findMinY lines) (findMaxY lines)

findMinY :: [Line] -> Y_cord
findMinY lines = findMinY_aux lines 9999999

findMinY_aux ::[Line] -> Y_cord -> Y_cord
findMinY_aux [] min = min
findMinY_aux ((p1,p2):lines) min = findMinY_aux lines (find_min p1 p2 min)
									 where find_min = (\(x1,y1) (x2,y2) m -> if y1 <= y2 then (if y1 < m then y1 else m )
																			 else (if y2 < m then y2 else m))

findMaxY :: [Line] -> Y_cord
findMaxY lines = findMaxY_aux lines (-1)

findMaxY_aux :: [Line] -> Y_cord -> Y_cord
findMaxY_aux [] max = max
findMaxY_aux ((p1,p2):lines) max = findMaxY_aux lines (find_max p1 p2 max)
									 where find_max = (\(x1,y1) (x2,y2) m -> if y1 >= y2 then (if y1 > m then y1 else m )
																		     else (if y2 > m then y2 else m))
																			 
traverseShapesAndDetect :: [ShapeMap] -> Map.Map Name [Name] -> Map.Map Name [Name]
traverseShapesAndDetect [] a = a
traverseShapesAndDetect (shapeM:shapesM) a = traverseShapesAndDetect shapesM $ detect shapeM shapesM a

detect :: ShapeMap -> [ShapeMap] -> Map.Map Name [Name] -> Map.Map Name [Name]
detect shapeM shapesM a = foldl (detect_aux shapeM) a shapesM
							where detect_aux = (\(n1,m1) a (n2,m2) -> if (compareMaps m1 m2)
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

-- makes Canvas an instance of Monoid
instance Monoid a => Monoid (Canvas a) where 
	mempty = return mempty
	mappend = liftM2 mappend

data Evnt a = Evnt a
	deriving (Eq, Ord, Show)
	
data Dynamic a = Dynamic { era        		:: Era
						 , originMap 		:: Map.Map Name (Time,OriginFn)
						 , eventOriginMap 	:: Map.Map Name [(Name,OriginFn)]
                         , runDynamic 		:: Time -> Dynamic a -> a
						 , boundaryMap 		:: Map.Map Name BoundaryFn
                         }
						 
getShapes :: Dynamic a -> Time -> [Shape]
getShapes d t = map (\(n,bfn) -> (n, bfn ((\(Just (st,ofn))-> ofn st (fromTime t)) (Map.lookup n (originMap d)))))  $ Map.toList $ boundaryMap d

--changeDynamic d t eventMap = d {originMap = Map.fromList [("newDynamic1", ( t ,originFn2)),("newDynamic2",(t,originFn3))]}
changeDynamic d t eventMap = if (Map.null (eventOriginMap d)) || (Map.null eventMap) then d else foldl (updateFn t) d $ Map.toList eventMap


updateFn :: Time -> Dynamic a -> (Name ,[Name]) -> Dynamic a
updateFn t d (n,evntlist)= if (null oFns) then d else modifyOriginMap d n timeOFNTuple
								where
									timeOFNTuple= (t, (head oFns))
									oFns = getOFNs evntlist $ Map.lookup n (eventOriginMap d)

getOFNs :: [Name] -> Maybe [(Name,OriginFn)] -> [OriginFn]
getOFNs _ Nothing = []
getOFNs evntList (Just evntOriginList) = map snd.head $ map ( \eventName -> filter ( \ (n,ofn) -> n == eventName) evntOriginList ) evntList

modifyOriginMap :: Dynamic a -> Name -> ( Time, OriginFn) -> Dynamic a
modifyOriginMap d name timeOFNTuple = d {originMap =Map.insert name timeOFNTuple (originMap d)}

simulate rate d = simulate_aux rate d (start (era d)) (end (era d))

simulate_aux rate d t e= if t > e then [] else (runDynamic d) t d : simulate_aux rate changedDynamic (t + (1^/rate)) e
							where
								--changedDynamic =if (fromTime t) /= 4 then d else d {originMap = Map.fromList [("newDynamic1", ( t ,originFn2)),("newDynamic2",(t,originFn3))]}
								changedDynamic = changeDynamic d t $ traverseShapesAndDetect (shapesToShapeMaps (getShapes d t)) Map.empty

newDynamic1 :: Dynamic (Canvas ())
newDynamic1 = Dynamic { era = mkEra (toTime 0) (toTime 10)
						, originMap = Map.fromList [("newDynamic1", ((toTime 0),originFn1)),("newDynamic2",((toTime 0),originFn3))]
						, eventOriginMap = Map.fromList [("newDynamic1",[("newDynamic2",originFn2)]),("newDynamic2",[("newDynamic1",originFn4)])]
						, runDynamic =	((\t d -> square1 $ (\(Just (st,ofn)) -> ofn st t) $ (Map.lookup "newDynamic1" (originMap d))) <> (\t d -> square2 $ (\(Just (st,ofn)) -> ofn st t) $ (Map.lookup "newDynamic2" (originMap d))))
						, boundaryMap = Map.fromList [("newDynamic1",square1BFn),("newDynamic2",square1BFn)]
						}

						
-- main function which starts the animation
main :: IO ()
main = blankCanvas 3000 $ \ context -> play context $ simulate (toRational 30) newDynamic1 

play context [] = do { send context $ do {fillText ("The End",650,250)}}
play context (n:ns) = do{
				send context $ do {
					render n};
				play context ns}


originFn1 :: Time -> Time -> Origin
originFn1 startTime currTime = ((400 + ( 50 *(fromTime currTime))) , 300)

originFn2 :: Time -> Time -> Origin
originFn2 startTime currTime = (((400+(50 * (fromTime startTime)))  - (((fromTime currTime) - (fromTime startTime))* 50)),300)

originFn3 :: Time -> Time -> Origin
originFn3 startTime currTime = ((900 - ( 50 *(fromTime currTime))) , 300)

originFn4:: Time -> Time -> Origin
originFn4 startTime currTime = (startXPosition + (time * 50), 300)
								where
									startXPosition= (900 - (50 * (fromTime startTime)))
									time = (fromTime currTime) - (fromTime startTime)

square1BFn :: Origin -> [Line]
square1BFn (x,y) = [((x,y),(x+100,y)),((x+100,y),(x+100,y+100)),((x+100,y+100),(x,y+100)),((x,y+100),(x,y))]

square1 :: Origin -> Canvas ()
square1 (x,y) = square x y 100 "Red"

square2 :: Origin -> Canvas ()
square2 (x,y) = square x y 100 "Blue"

