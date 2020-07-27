module Operator (Operator, initOperator) where

import Matrix
import Maths

data Operator = Operator {matrix :: Matrix} deriving (Show)

--Will need to check to make sure that matrix is unitary
initOperator :: Matrix -> Operator
initOperator m
	| isUnitary m = Operator m
	| otherwise = error $ "Matrix" ++ show m ++ "is not unitary!"
