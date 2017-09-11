import System.Environment
import System.IO  
import Data.List  
  
import Swapswap


main = do
    (command:strs) <- getArgs  
    putStrLn "Possible decrypted text variants:"
    let (Just decrypter) = lookup command dispatch  
    putStrLn (decrypter (joinStrings strs))


-- Common --

minDivisor = 4
maxLinesCountScyt = 20
maxLinesCountTransp = 8


dispatch :: [( String, String -> String )]  
dispatch =  [ ("scyt", decryptScytale)  
            , ("transp", decryptTransposition)  
            , ("fence", decryptFence)
            ]  


getDivisors :: Int -> [Int]
getDivisors l = [ x | x <- [minDivisor..(l `div` 3)], l `mod` x == 0 ]


removeSpaces :: String -> String
removeSpaces str = [ s | s <- str, s /= ' ' ]


splitBy :: Int -> String -> [String]
splitBy step str = 
    if length str <= step 
        then [str] 
        else (take step str):(splitBy step (drop step str))


getVariants :: String -> Int -> [(Int, [String])]
getVariants str maxLinesCount = filter (\var -> length (snd var) < maxLinesCount) [ (step, splitBy step str) | step <- getDivisors (length str)]


getFirstLength :: [String] -> Int
getFirstLength strs = length (head strs)


getFromAllByIdx :: [String] -> Int -> String
getFromAllByIdx strs idx =
    if length strs < 1
        then []
        else ((head strs) !! idx) : (getFromAllByIdx (tail strs) idx)


concatVertically :: [String] -> [String]
concatVertically strs = 
    [ getFromAllByIdx strs idx | idx <- [0..((getFirstLength strs)-1)] ]


joinStrings :: [String] -> String
joinStrings strs = foldl (\acc x -> acc ++ x) "" strs


getTransposedString :: [String] -> String
getTransposedString strs = 
    joinStrings (concatVertically strs)


-- Scytale --

getDecryptedScytaleVariants :: String -> [(Int, String)]
getDecryptedScytaleVariants str = 
    [ ( step, getTransposedString var ) | (step, var) <- (getVariants str maxLinesCountScyt) ]


formatScytaleVariants :: [(Int, String)] -> String
formatScytaleVariants variants = foldl (\acc (step, str) -> acc ++ show(step) ++ " " ++ str ++ "\n") "\n" variants


decryptScytale :: String -> String
decryptScytale str = formatScytaleVariants (getDecryptedScytaleVariants str)


-- Transposition --

getAllPossibleOrders :: Int -> [[Int]]
getAllPossibleOrders l = swapswap [0..l-1]


reorderVariant :: [Int] -> [String] -> [String]
reorderVariant linesOrder variant =
    [ variant !! idx | idx <- linesOrder ]


getPossibleOrdersForVariant :: Int -> [String] -> [(Int, [Int], [String])]
getPossibleOrdersForVariant step variant = 
    [ (step, linesOrder, reorderVariant linesOrder variant) | linesOrder <- (getAllPossibleOrders (length variant)) ]


flattenTranspositionVariants :: [[(Int, [Int], [String])]] -> [(Int, [Int], [String])]
flattenTranspositionVariants tvs = 
    foldl (\acc tv -> acc ++ tv) [] tvs


getTranspositionVariants :: String -> [(Int, [Int], [String])]
getTranspositionVariants str = 
    flattenTranspositionVariants [ getPossibleOrdersForVariant step variant | (step, variant) <- (getVariants str maxLinesCountTransp) ]


getDecryptedTranspositionVariants :: String -> [(Int, [Int], String)]
getDecryptedTranspositionVariants str = 
    [ ( step, linesOrder, getTransposedString var ) | (step, linesOrder, var) <- (getTranspositionVariants str) ]


formatOrder :: [Int] -> String
formatOrder linesOrder = foldl (\acc i -> acc ++ show(i)  ++ "-") "-" linesOrder


formatTranspositionVariants :: [(Int, [Int], String)] -> String
formatTranspositionVariants variants = foldl (\acc (step, linesOrder, str) -> acc ++ show(step) ++ " " ++ (formatOrder linesOrder) ++ "\n" ++ str ++ "\n\n") "\n" variants


decryptTransposition :: String -> String
decryptTransposition str = formatTranspositionVariants (getDecryptedTranspositionVariants str)


-- Fence --

fenceHeight = 4

getFractionalLength :: String -> Rational
getFractionalLength str = fromIntegral (length str)


getPartsCount :: Int -> String -> Int
getPartsCount height str = ceiling ((getFractionalLength str) / (fromIntegral height))


decryptFenceWithKey :: Int -> String -> [String]
decryptFenceWithKey height str = splitBy (getPartsCount height str) str


formatFenceResult :: [String] -> String
formatFenceResult parts = foldl (\acc s -> acc ++ s ++ "\n") "\n" parts


decryptFence :: String -> String
decryptFence str = formatFenceResult (decryptFenceWithKey fenceHeight str)

