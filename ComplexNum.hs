module ComplexNum (ComplexNum(..), angle) where

data ComplexNum = ComplexNum {real :: Double, imaginary :: Double}

instance Num ComplexNum where
	(ComplexNum a b) + (ComplexNum c d) = (ComplexNum (a + c) (b + d))
	(ComplexNum a b) * (ComplexNum c d) = (ComplexNum ((a * c) - (b * d)) ((a * d) + (b * c)))
	abs (ComplexNum a b) = (ComplexNum (sqrt ((a * a) + (b * b))) 0)
	signum (ComplexNum a b) = (ComplexNum (signum a) (signum b))
	fromInteger a = (ComplexNum (fromIntegral a) 0)
	negate (ComplexNum a b) = (ComplexNum (-a) (-b))

instance Eq ComplexNum where
	(==) (ComplexNum a b) (ComplexNum c d) = (a == c) && (b == d)

instance Show ComplexNum where
	show (ComplexNum r i) = (show r) ++ "+" ++ (show i) ++ "i"

angle :: ComplexNum -> Double
angle c = atan2 (imaginary c) (real c)
