module QuantumRegister (QuantumRegister, initRegister) where

import Qbit
import ComplexNum
import Data.List

data QuantumRegister = QuantumRegister {bits :: [Qbit]}

data State = State {value :: [Char], probDens :: ComplexNum}

instance Show QuantumRegister where
	show (QuantumRegister bits) = showReg bits

instance Eq State where
	(==) (State v _) (State w _) = (read v :: Int) == (read w :: Int)

instance Ord State where
	(>) (State v _) (State w _) = v > w
	(<=) (State v _) (State w _) = v <= w
	compare (State v _) (State w _)
		| v < w = LT
		| v > w = GT
		| otherwise = EQ

showReg :: [Qbit] -> [Char]
showReg bits = showStates (sort (genStates bits))

genStates :: [Qbit] -> [State]
genStates (x:[]) = [(State ['0'] ((state x) !! 0)), (State ['1'] ((state x) !! 1))]
genStates (x:xs) = [(State ((value s) ++ ['0']) (((state x) !! 0) * (probDens s))) | s <- (genStates xs)] ++ [(State ((value t) ++ ['1']) (((state x) !! 1) * (probDens t))) | t <- (genStates xs)]

showStates :: [State] -> [Char]
showStates (x:[]) = (show (probDens x)) ++ "|" ++ (value x) ++ ">"
showStates (x:xs) = (show (probDens x)) ++ "|" ++ (value x) ++ "> + " ++ (showStates xs)

initRegister :: Int -> QuantumRegister
initRegister n = (QuantumRegister (take n (repeat (initQbit 1 0))))
