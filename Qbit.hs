module Qbit (Qbit, state, initQbit, phase) where

import ComplexNum

data Qbit = Qbit {state :: [ComplexNum]}

instance Show Qbit where
	show (Qbit state) = (show (state !! 0)) ++ "|0> + " ++ (show (state !! 1)) ++ "|1>"

initQbit :: ComplexNum -> ComplexNum -> Qbit
initQbit x y = (Qbit [x, y])

phase :: Qbit -> Double
phase c = (angle ((state c) !! 0)) - (angle ((state c) !! 1))
