{-# LANGUAGE DeriveFunctor
           , GeneralizedNewtypeDeriving
           , TypeSynonymInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , FlexibleInstances
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Active_lib where
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.List
import Graphics.Blank
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
type OriginFn = Time -> Point-> Time -> Origin
type CanvasFn a = Origin -> a
type Line=(Point,Point)
type Shape=(Name,[Line])
type ShapeMap=(Name,Map.Map Y_cord (X_cord,X_cord))
type Eqn = Y_cord -> (X_cord,X_cord)
type ActEvent = Name
type Delay =Duration
type EventAction = (ActEvent,OriginFn)
type ShapeFn a = Origin -> a
type BoundaryFn = Origin -> [Line]
	
									  
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
instance Monoid a => Semigroup (Canvas a) where
	(<>) = mappend

instance Semigroup a => Semigroup (Dynamic a) where
  Dynamic e1 om1 eom1 rd1 bm1 <> Dynamic e2 om2 eom2 rd2 bm2 = Dynamic (e1 <> e2) (Map.union om1 om2) (Map.union eom1 eom2) (rd1 <> rd2) (Map.union bm1 bm2)

data Evnt a = Evnt a
	deriving (Eq, Ord, Show)
	
data Dynamic a = Dynamic { era        		:: Era
						 , originMap 		:: Map.Map Name ((Time,Point),OriginFn)
						 , eventOriginMap 	:: Map.Map Name [(ActEvent,OriginFn)]
                         , runDynamic 		:: Time -> Dynamic a -> a
						 , boundaryMap 		:: Map.Map Name (BoundaryFn,(Time,Time))
                         }
						 
mkDynamic :: Name ->ShapeFn a-> BoundaryFn -> Double -> Double -> OriginFn -> [EventAction] -> Dynamic a
mkDynamic name sFn bFn start end oFn evntActs = Dynamic { era = mkEra (toTime start) (toTime end)
														, originMap = Map.fromList [(name,((toTime 0,(-1,-1)),oFn))]
														, eventOriginMap = Map.fromList [(name,evntActs)]
														, runDynamic =(\t d -> sFn $ (\ ((st,sp),ofn) ->ofn st sp t) $ fromJust $ Map.lookup name (originMap d))
														, boundaryMap = Map.fromList [(name,(bFn,(toTime (-1),toTime (99999999))))]}
														
shiftDynamic :: Duration -> Dynamic a -> Dynamic a
shiftDynamic sh dy = dy { era = mkEra (st .+^ sh) (en .+^ sh)
					  , runDynamic = (\ t d -> (runDynamic dy) (t .-^ sh) d)
					  , originMap = Map.map (\((st,sp),ofn) -> ((st .+^ sh ,sp),ofn)) (originMap dy)
					  , boundaryMap = Map.map (\(bFn,(s,e)) -> (bFn,(if (fromTime s) == -1 then (st .+^ sh) else s .+^ sh,if (fromTime e) == 99999999 then e else (e .+^ sh)))) (boundaryMap dy)}
						where 
							st = (start $ era dy)
							en = (end $ era dy)
					  
newtype Active a = Active (MaybeApply Dynamic a)

instance Newtype (Active a) (MaybeApply Dynamic a) where
  pack              = Active
  unpack (Active m) = m

instance Newtype (MaybeApply f a) (Either (f a) a) where
  pack   = MaybeApply
  unpack = runMaybeApply
  
over2 :: (Newtype n o, Newtype n' o', Newtype n'' o'')
      => (o -> n) -> (o -> o' -> o'') -> (n -> n' -> n'')
over2 _ f n1 n2 = pack (f (unpack n1) (unpack n2))

instance Semigroup a => Semigroup (Active a) where
  (<>) = (over2 Active . over2 MaybeApply) combine
   where
    combine (Right m1) (Right m2)
      = Right (m1 <> m2)

    combine (Left d) (Right m)
      = Left (d {runDynamic = (runDynamic d) <> (\_ _ -> m)})

    combine (Right m) (Left d)
      = Left (d {runDynamic = (\_ _ -> m) <> (runDynamic d)})

    combine (Left d1) (Left d2)
      = Left (d1 <> d2)

instance (Monoid a, Semigroup a) => Monoid (Active a) where
  mempty  = Active (MaybeApply (Right mempty))
  mappend = (<>)
  
-- | Create an 'Active' value from a 'Dynamic'.
fromDynamic :: Dynamic a -> Active a
fromDynamic = Active . MaybeApply . Left

mkActive ::Name ->ShapeFn a-> BoundaryFn -> Double -> Double -> OriginFn -> [EventAction] -> Active a
mkActive name sFn bFn start end oFn evntActs = fromDynamic (mkDynamic name sFn bFn start end oFn evntActs)

onActive :: (a -> b) -> (Dynamic a -> b) -> Active a -> b
onActive f _ (Active (MaybeApply (Right a))) = f a
onActive _ f (Active (MaybeApply (Left d)))  = f d

modActive :: (a -> b) -> (Dynamic a -> Dynamic b) -> Active a -> Active b
modActive f g = onActive (Active . MaybeApply . Right . f) (fromDynamic . g)

runActive :: Active a -> (Time -> Dynamic a -> a)
runActive = onActive (\ a _ _ -> a) runDynamic

activeEra :: Active a -> Maybe Era
activeEra = onActive (const Nothing) (Just . era)

isConstant :: Active a -> Bool
isConstant = onActive (const True) (const False)

-- | Test whether an 'Active' value is 'Dynamic'.
isDynamic :: Active a -> Bool
isDynamic = onActive (const False) (const True)

------------------------------------------------------------
--  Combinators
------------------------------------------------------------

stretch :: Rational -> Active a -> Active a
stretch str = modActive id (\dy -> dy { era = mkEra (start $ era dy) ((start $ era dy) .+^ (str *^ ((end $ era dy) .-. (start $ era dy))))
									  , runDynamic = (\ t d -> (runDynamic dy) ((start $ era dy) .+^ ((t .-. (start $ era dy)) ^/ str)) d)})
										
stretchTo :: Duration -> Active a -> Active a
stretchTo d a
  | d <= 0                               = a
  | (duration <$> activeEra a) == Just 0 = a
  | otherwise = maybe a (`stretch` a) ((toRational . (d /) . duration) <$> activeEra a)
  
shift :: Duration -> Active a -> Active a
shift sh = modActive id (shiftDynamic sh)

trim :: Monoid a => Active a -> Active a
trim = modActive id (\dy -> dy { runDynamic = (\ t d -> case () of _ | t < (start $ era dy) -> mempty
																	 | t > (end $ era dy)   -> mempty
																	 | otherwise -> (runDynamic dy) t d)
							   , boundaryMap = Map.map (\(bFn,(s,e)) -> (bFn,(if (fromTime s) == -1 then (start $ era dy) else s,if (fromTime e) == 99999999 then (end $ era dy) else e))) (boundaryMap dy)})
trimBefore :: Monoid a => Active a -> Active a
trimBefore = modActive id (\dy -> dy { runDynamic = (\ t d -> case () of _ | t < (start $ era dy) -> mempty
																		   | otherwise -> (runDynamic dy) t d)
									 , boundaryMap = Map.map (\(bFn,(s,e)) -> (bFn,(if (fromTime s) == -1 then (start $ era dy) else s,e))) (boundaryMap dy)})

trimAfter :: Monoid a => Active a -> Active a
trimAfter = modActive id (\dy -> dy { runDynamic = (\ t d -> case () of _ | t > (end $ era dy) -> mempty
																		  | otherwise -> (runDynamic dy) t d)
									, boundaryMap = Map.map (\(bFn,(s,e)) -> (bFn,(s,if (fromTime e) == 99999999 then (end $ era dy) else e))) (boundaryMap dy)})

setEra :: Era -> Active a -> Active a
setEra er = modActive id (\dy -> dy { era = er})

atTime :: Time -> Active a -> Active a
atTime t a = maybe a (\e -> shift (t .-. start e) a) (activeEra a)

after :: Active a -> Active a -> Active a
after a1 a2 = maybe a1 ((`atTime` a1) . end) (activeEra a2)

(<=>) :: (Monoid a , Semigroup a) => Active a -> Active a -> Active a
a1 <=> a2 = a1 <> a2

(->>) :: Semigroup a => Active a -> Active a -> Active a
a1 ->> a2 = a1 <> (a2 `after` a1)

(|>>) :: (Monoid a , Semigroup a) => Active a -> Active a -> Active a
a1 |>> a2 = (trimAfter a1) ->> (trimBefore a2)

movie :: (Monoid a , Semigroup a) => [Active a] -> Active a
movie = foldr1 (|>>)

getDelay :: Name -> Dynamic a -> Time
getDelay n d = if delay == -1 then toTime 0 else delay 
				where 
					delay = fst $ snd $ fromJust $ Map.lookup n $ boundaryMap d
						 
getShapes :: Dynamic a -> Time -> [Shape]
getShapes d t = map (\(n,(bfn,(s,e))) -> if (fromTime t) > (fromTime s) && (fromTime t) < (fromTime e) then (n, bfn ((\(Just ((st,sp),ofn))-> ofn st sp $ fromTime $ t - s) (Map.lookup n $ originMap d))) else (n,[]))  $ Map.toList $ boundaryMap d

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
							else if (length oFns) == 0 then d else modifyOriginMap d n ((time,startPosition), (head oFns))
								where
									((st,sp),currentOFn)= fromJust $ Map.lookup n $ originMap d
									time = t - (getDelay n d)
									startPosition = currentOFn st sp time									
									oFns = getOFNs evntlist $ Map.lookup n (eventOriginMap d)
									getOFNs _ Nothing = []
									getOFNs [] _ = []
									getOFNs evntList (Just evntOriginList) = map snd.head $ map ( \eventName -> filter ( \ (n,ofn) -> n == eventName) evntOriginList ) evntList 									

modifyOriginMap :: Dynamic a -> Name -> ((Time,Point), OriginFn) -> Dynamic a
modifyOriginMap d name timeOFNTuple = d {originMap =Map.insert name timeOFNTuple (originMap d)}

simulateActive :: Rational -> Active a -> [(Time,a)]
simulateActive rate = onActive (\a -> zip (map toTime [0,(0+(1/(fromRational rate)))]) (repeat a)) (\d -> simulate rate d)

simulate rate d = simulate_aux rate d (start (era d)) (end (era d)) Map.empty

simulate_aux rate d t e oldEventMap = if t > e then [] else (t,currentCanvas): simulate_aux rate changedDynamic (t + (1^/rate)) e newEventMap
							where
								currentCanvas = (runDynamic d) t d
								newEventMap = traverseShapesAndDetect (shapesToShapeMaps (getShapes d t)) Map.empty
								changedDynamic = changeDynamic d t oldEventMap newEventMap


-- newDynamic1 :: Dynamic (Canvas ())
-- newDynamic1 = Dynamic { era = mkEra (toTime 0) (toTime 10) <> mkEra (toTime 10) (toTime 15)
						-- , originMap = Map.fromList [("newDynamic1", ((toTime 0),originFn1))
													-- ,("newDynamic2",((toTime 0),originFn3))
													-- ,("newDynamic3",((toTime 0),originFn5))]
						-- , eventOriginMap = Map.fromList [("newDynamic1",[("newDynamic2",originFn2)]),("newDynamic2",[("newDynamic1",originFn4)])]
						-- , runDynamic =	((\t d -> square1 $ (\(Just (st,ofn)) -> ofn st t) $ (Map.lookup "newDynamic1" (originMap d))) <> (\t d -> square2 $ (\(Just (st,ofn)) -> ofn st (t .-^ (toDuration 5))) $ (Map.lookup "newDynamic2" (originMap d)))) <> (\t d -> square1 $ (\(Just (st,ofn)) -> ofn st t) $ (Map.lookup "newDynamic3" (originMap d)))
						-- , boundaryMap = Map.fromList [("newDynamic1",(square1BFn,toDuration 0)),("newDynamic2",(square1BFn,toDuration 5))]
						-- }
														


play context _ _ [] = do 
						send context $ do 
										clearRect (0,0,100,40)
										font $ Text.pack "20px Verdana"
										fillStyle $ Text.pack "blue"
										--fillText ((Text.pack "The End"),650,250)
										fillText ((Text.pack "Done"),20,20)
play context status intervals (n:ns)  = do{
				send context $ do {
					render (snd n) (width context) (height context);
					font $ Text.pack "10px Verdana";fillStyle $ Text.pack "blue";
					fillText ((Text.pack $ take 5 $ show $ fromTime $ fst n),20,50);
					if status == "Play" 
						then do {font $ Text.pack "20px Verdana";fillStyle $ Text.pack "green";fillText ((Text.pack "Playing"),20,20);};
						else do {font $ Text.pack "20px Verdana";fillStyle $ Text.pack "red";fillText ((Text.pack "Paused"),20,20);};
					};
				e <- atomically $ (fmap Just $ readTChan (eventQueue context)) `orElse` return Nothing ;
				if status == "Play" 
				then if (length intervals) > 0 && (fst n) >= (head intervals) 
						then play context "Pause" (tail intervals) (n:ns) 
						else if (show e) == "Nothing" then play context "Play" intervals ns else play context "Pause" intervals (n:ns)
				else if (show e) == "Nothing" then play context "Pause" intervals (n:ns) else play context "Play" intervals ns;
				}
				
render :: Canvas a -> Double ->Double -> Canvas ()
render r width height= do 
					clearRect (0,0,width,height)
					fillStyle $ Text.pack "white"
					fillRect (0,0,width,height)
					beginPath ()
					save ()
					r
					restore ()
