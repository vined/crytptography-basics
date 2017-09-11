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
getVariants str maxLinesCount =
    filter (\var -> length (snd var) < maxLinesCount) [ (step, splitBy step str) | step <- getDivisors (length str)]


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
formatTranspositionVariants variants =
    foldl (\acc (step, linesOrder, str) -> acc ++ show(step) ++ " " ++ (formatOrder linesOrder) ++ "\n" ++ str ++ "\n\n") "\n" variants


decryptTransposition :: String -> String
decryptTransposition str = formatTranspositionVariants (getDecryptedTranspositionVariants str)


-- Fence --

fenceHeight = 4

getStepSize :: Int -> Int
getStepSize height =
    if height <= 2
        then height
        else ((height - 2) * 2) + 2

getLastStepSize :: Int -> Int -> Int
getLastStepSize total stepSize = total `mod` stepSize

getWholeStepsCount :: Int -> Int -> Int
getWholeStepsCount total stepSize = total `div` stepSize


calculateStepElementsCount :: Int -> Int -> Int -> [Int]
calculateStepElementsCount stepSize lastStepSize wholeStepsCount =
    [ if x <= lastStepSize then (wholeStepsCount + 1) else wholeStepsCount | x <- [1..stepSize]]


getMiddle :: [a] -> [a]
getMiddle xs =
    if length xs < 2
        then []
        else tail (init xs)

sumFirstAndLast :: [Int] -> Int
sumFirstAndLast xs =
    case xs of
        [] -> 0
        [x] -> x
        xs -> (head xs) + (last xs)

mergeFirstWithLast :: [Int] -> [Int]
mergeFirstWithLast xs =
    if length xs < 2
        then xs
        else (sumFirstAndLast xs) : (mergeFirstWithLast (getMiddle xs))


getStepElementsCount :: Int -> Int -> [Int]
getStepElementsCount height total = let stepSize = getStepSize height in
    calculateStepElementsCount stepSize (getLastStepSize total stepSize) (getWholeStepsCount total stepSize)

getLinesElementsCount :: [Int] -> [Int]
getLinesElementsCount stepElementsCount =
    if length stepElementsCount <= 2
        then stepElementsCount
        else (head stepElementsCount) : (mergeFirstWithLast (tail stepElementsCount))


splitToLines :: [Int] -> String -> [String]
splitToLines [x] str = [str]
splitToLines linesSizes str =
    let lineSize = head linesSizes
    in (take lineSize str) : (splitToLines (tail linesSizes) (drop lineSize str))

getLines :: Int -> String -> [String]
getLines height str = splitToLines (getLinesElementsCount (getStepElementsCount height (length str))) str


getStringOrEmpty :: Int -> [String] -> String
getStringOrEmpty idx strs = if length strs <= idx then "" else strs !! idx

getCharOrEmpty :: Int -> String -> Char
getCharOrEmpty idx str = if length str <= idx then ' ' else (str !! idx)


preAppendEach :: String -> [String] -> [String]
preAppendEach appenders strs =
    let l = length appenders
    in [ (appenders !! i) : (getStringOrEmpty i strs) | i <- [0..l-1] ]

insertToMiddle :: String -> String -> String
insertToMiddle pair str = [(head pair)] ++ str ++ [(last pair)]

insertToMiddleForEachPair :: [String] -> [String] -> [String]
insertToMiddleForEachPair pairs inserts =
    let l = length pairs
    in [ insertToMiddle (pairs !! i) (getStringOrEmpty i inserts) | i <- [0..l-1] ]

splitToPairs :: String -> [String]
splitToPairs str =
    let l = length str
    in [ [(getCharOrEmpty i str), (getCharOrEmpty (i+1) str)] | i <- [0,2..l-1] ]

mergeLines :: [String] -> [String]
mergeLines [str] = [ [s] | s <- str ]
mergeLines strs = insertToMiddleForEachPair (splitToPairs (head strs)) (mergeLines (tail strs))


getDecryptedSteps :: Int -> String -> [String]
getDecryptedSteps height strs =
    let lns = getLines height strs
    in preAppendEach (head lns) (mergeLines (tail lns))


decryptFenceWithHeight :: Int -> String -> String
decryptFenceWithHeight height str = joinStrings (getDecryptedSteps height str)

decryptFence :: String -> String
decryptFence str = decryptFenceWithHeight fenceHeight str
