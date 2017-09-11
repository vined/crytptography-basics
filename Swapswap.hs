module Swapswap where

swapLast :: [Int] -> [Int]
swapLast x = 
    if length x <= 2
        then reverse x
        else (head x) : reverse (tail x)


swapFirstWithIdx :: Int -> [Int] -> [Int]
swapFirstWithIdx idx x = (x !! idx) : (take (idx) x ++ drop (idx+1) x)


swapFirstWithAll :: [Int] -> [[Int]]
swapFirstWithAll x =
    [ swapFirstWithIdx idx x | idx <- [0..(length x)-1] ]


swapswapTails :: [Int] -> [[Int]]
swapswapTails xs =
    map (head xs :) (swapswap (tail xs))


swapswap :: [Int] -> [[Int]]
swapswap xs =
    case xs of [x] -> [[x]]
               [x,y] -> [[x,y], [y,x]]
               [x,y,z] -> concat (map (\arr -> arr : (swapLast arr) : []) (swapFirstWithAll [x,y,z]))
               xs -> concat (map swapswapTails (swapFirstWithAll xs))
