module Maths (dotProduct) where

import ComplexNum

dotProduct :: [ComplexNum] -> [ComplexNum] -> ComplexNum
dotProduct [] [] = 0
dotProduct (x:xs) (y:ys) = (x * y) + (dotProduct xs ys)
