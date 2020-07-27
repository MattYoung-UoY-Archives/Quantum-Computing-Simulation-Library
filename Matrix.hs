module Matrix (Matrix(..), getValue, transpose) where

import ComplexNum
import Maths

data Matrix = Matrix {values :: [[ComplexNum]]}

instance Show Matrix where
	show (Matrix m) = ("\n" ++ printRows m)

instance Num Matrix where
	(Matrix a) + (Matrix b) = (Matrix (addMatrices a b))
	a * b = (Matrix (multMatrices a b))
	abs _ = undefined
	signum _ = undefined
	negate (Matrix a) = (Matrix [[(-y) | y <- x] | x <- a])
	fromInteger _ = undefined

addMatrices :: [[ComplexNum]] -> [[ComplexNum]] -> [[ComplexNum]]
addMatrices [] [] = []
addMatrices (x:xs) (y:ys) = (addLists x y) : (addMatrices xs ys)
addMatrices _ _ = undefined

multMatrices :: Matrix -> Matrix -> [[ComplexNum]]
multMatrices (Matrix a) (Matrix b) = matrixMul a b

matrixMul :: [[ComplexNum]] -> [[ComplexNum]] -> [[ComplexNum]]
matrixMul [] b = []
matrixMul (x:xs) b = (matMulRow x b ((length (b !! 0)) - 1)) : (matrixMul xs b)

matMulRow :: [ComplexNum] -> [[ComplexNum]] -> Int -> [ComplexNum]
matMulRow x b (-1) = []
matMulRow x b i = (dotProduct x (getCol b ((length (b !! 0)) - (i + 1)))) : (matMulRow x b (i - 1))

addLists :: [ComplexNum] -> [ComplexNum] -> [ComplexNum]
addLists [] [] = []
addLists (x:xs) (y:ys) = (x+y) : (addLists xs ys)
addLists _ _ = undefined

printRows :: [[ComplexNum]] -> String
printRows [] = ""
printRows (x:xs) = "[ " ++ (printCols x) ++ " ]\n" ++ (printRows xs)

printCols :: [ComplexNum] -> String
printCols [] = ""
printCols (x:[]) = (show x)
printCols (x:xs) = (show x) ++ " | " ++ (printCols xs)

getCol :: [[ComplexNum]] -> Int -> [ComplexNum]
getCol [] i = []
getCol (x:xs) i = (x !! i) : getCol xs i

getValue :: Matrix -> Int -> Int -> ComplexNum
getValue (Matrix m) i j = (m !! j) !! i

transpose :: Matrix -> Matrix
transpose (Matrix m) = (Matrix (reverse (tnsp m)))

tnsp :: [[ComplexNum]] -> [[ComplexNum]]
tnsp [] = []
tnsp (x:xs) = (reverse x) : (tnsp xs)

--Needs Completing
isUnitary :: Matrix -> Bool
isUnitary (Matrix m) = True
