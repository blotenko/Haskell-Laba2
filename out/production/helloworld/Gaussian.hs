module Gaussian where
import Data.List (elemIndices)
import GHC.Conc ( pseq )
import GHC.Conc.Sync (par)

type Row = [Float]
type Matrix = [Row]
type Vector = Row
type Point = (Int, Int)

minusInf :: Float
minusInf = read "-Infinity"

eps :: Float
eps = 1e-10

-- Функции для работы с матрицами и векторами

subRows :: Row -> Row -> Row
subRows = zipWith (-)

multiplyRowNumber :: Row -> Float -> Row
multiplyRowNumber row value = map (* value) row

valueToMul :: Int -> Row -> Row -> Float
valueToMul ind x y
    | ind < length x && ind < length y && ind >= 0 = x !! ind / y !! ind
    | otherwise = 0

changeValue :: Vector -> Float -> Int -> Vector
changeValue [] _ _ = []
changeValue vec newValue ind
    | ind < length vec && ind >= 0 = take ind vec ++ [newValue] ++ drop (ind + 1) vec
    | otherwise = vec

customMax :: Row -> Float
customMax [] = minusInf
customMax [_] = minusInf
customMax (x:xs)
    | x > val = x
    | otherwise = val
    where
        val = customMax xs

maxIndex :: Matrix -> [Int] -> Float -> Int -> Point -> Point
maxIndex [] _ _ _ ind = ind
maxIndex (x:xs) usedRows value rowId ind
    | elem rowId usedRows || value >= customMax x = maxIndex xs usedRows value (rowId + 1) ind
    | value < customMax x && customMax x /= 0 = maxIndex xs usedRows (customMax x) (rowId + 1) (rowId, newInd)
    | otherwise = (-1, -1)
    where
        newInd = head $ elemIndices (customMax x) x

-- Функции для преобразования матрицы

goAlongRows :: Matrix -> Int -> Row -> Point -> Matrix
goAlongRows [] _ _ _ = []
goAlongRows (mat:mats) rowInd rowToSub maxInd@(maxRowInd, maxColInd)
    | maxRowInd == rowInd = nextRowsResult `par` (force divOnOwn `pseq` (divOnOwn : nextRowsResult))
    | otherwise = subResult : nextRowsResult
    where
        subResult = if checkLessZero res
                    then multiplyRowNumber res (-1)
                    else res
        divOnOwn = multiplyRowNumber mat (1 / (mat !! maxColInd))
        nextRowsResult = goAlongRows mats (rowInd + 1) rowToSub maxInd
        res = subRows mat (multiplyRowNumber rowToSub (valueToMul maxColInd mat rowToSub))
        checkLessZero :: Row -> Bool
        checkLessZero [] = True
        checkLessZero [_] = True
        checkLessZero (xRow:xsRow)
            | xRow > 0 = False
            | otherwise = checkLessZero xsRow

force :: [a] -> ()
force = foldr pseq ()

addVectorToMartix :: Matrix -> Vector -> Matrix
addVectorToMartix [] [] = []
addVectorToMartix [] _ = []
addVectorToMartix _ [] = []
addVectorToMartix (mat:mats) (vec:vecs) = (mat ++ [vec]) : addVectorToMartix mats vecs

findMainPivot :: Matrix -> [Int] -> Matrix
findMainPivot mat usedRows
    | row == -1 || col == -1 = mat
    | length usedRows /= length mat = findMainPivot (goAlongRows mat 0 rowToSub maxInd) (row : usedRows)
    | otherwise = mat
    where
        maxInd@(row, col) = maxIndex mat usedRows minusInf 0 (-1, -1)
        rowToSub = mat !! row

data CustomAns = Exists Vector
                | MinusInf
                | NotExists
    deriving (Show, Eq)

-- Главная функция Гаусса

gaussian :: Matrix -> Vector -> CustomAns
gaussian [] _ = NotExists
gaussian mat vec = checkResult $ findMainPivot (addVectorToMartix mat vec) []

-- Функции для проверки результата

checkRowResult :: Row -> Int
checkRowResult (x:xs)
    | null xs && abs x < eps = -1
    | null xs = 0
    | abs x < eps = checkRowResult xs
    | otherwise = 1

checkResult :: Matrix -> CustomAns
checkResult [] = Exists []
checkResult (mat:mats)
    | ans == NotExists = NotExists
    | ans == MinusInf = MinusInf
    | curCheck == 0 = NotExists
    | curCheck == -1 = MinusInf
    | otherwise = Exists value
    where
        curCheck = checkRowResult mat
        ans = checkResult mats
        value = case ans of
            Exists a -> last mat : a
            _ -> []



