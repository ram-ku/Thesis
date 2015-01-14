{-# LANGUAGE Arrows #-}

module Box where

import FRP.Yampa

type Acceleration = Double
type Velocity     = Double
type Position     = Double
type Radius       = Double
type Height		  = Double
type Width		  = Double
type Color		  = String

data Box         = Box { boxHeight  :: Height
						, boxWidth 	:: Width
                        , boxPosX 	:: Position
                        , boxPosY 	:: Position
                        , boxVel  	:: Velocity
						, boxCol  	:: Color
                         }
-------------------------------------------------------------------------
movingBox :: Box -> SF a Box
movingBox b = let v0 = boxVel b
                  x0 = boxPosX b
                 in
                    proc _ -> do
						x <- iIntegral x0 -< v0
						returnA			-< b { boxVel = v0, boxPosX = if x <= 0.7 then x else 0.7}

detectBounce :: SF Box (Event Box)
detectBounce = proc b -> do
                 e <- edge  -<  (boxPosX b + 0.008) >= 0.6 && boxPosX b <= 0.65
                 returnA    -<  tag e (b {boxPosX = 0.64})

bouncingBox :: Box -> SF a Box
bouncingBox b = switchWhen (movingBox b) detectBounce bouncingBox

-------------------------------------------------------------------------
box1 :: Box
box1 = Box { boxHeight  = 0.06
            , boxWidth = 0.01
            , boxPosX = 0.5
			, boxPosY = 0.5
            , boxVel  = 0.02
		    , boxCol  = "black"
             }
-------------------------------------------------------------------------

-- Utilities

switchWhen :: SF a b -> SF b (Event e) -> (e -> SF a b) -> SF a b
switchWhen sf sfe = switch (sf >>> (identity &&& sfe))

iIntegral :: VectorSpace x s => x -> SF x x
iIntegral x = integral >>> arr (^+^ x)

-------------------------------------------------------------------------
