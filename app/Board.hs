module Board (
    CellValue,
    CellRegion,
    Pos,
    Cell(..),
    makeBoard,
    fillMandatory,
    printBoard,
    getPossibleValues,
    getLowerRelative,
    getFamily,
    isFamily,
    getNeighbors,
    getUpperRelative,
    fillVerticals,
    isVertical,
    getFreeVertical,
    initialFill
) where

import Data.List
import Data.Matrix
import Data.Maybe
import Debug.Trace
import GHC.Arr

type CellValue = Int -- Used to represent the value of a cell
type CellRegion = Int -- Used to represent the region of a cell
type Pos = (Int, Int)

data Cell = Cell {
    value :: CellValue
    , definitive :: Bool
    , region :: CellRegion
    }

newCell :: (CellValue, CellRegion) -> Cell
newCell (value, region) = Cell {
    value = value
    , definitive = value /= 0
    , region = region
    }

makeBoard :: [[(CellValue, CellRegion)]] -> Matrix Cell
makeBoard tuples = fromLists $ map (map newCell) tuples

-- Get a list containing the values of the neighbors of a cell
getNeighbors :: Matrix Cell -> Pos -> [CellValue]
getNeighbors board (x, y) = [a,b,c,d] where
    a = maybe 0 value (safeGet x (y-1) board)
    b = maybe 0 value (safeGet x (y+1) board)
    c = maybe 0 value (safeGet (x-1) y board)
    d = maybe 0 value (safeGet (x+1) y board)


isFamily :: Cell -> Cell -> Bool
isFamily cell1 cell2 = region cell1 == region cell2


-- | Get the cells in the same region as the cell
-- Warning: this function can be optimized
getFamily :: Matrix Cell -> Pos -> [Cell]
getFamily board (x, y) = filter (isFamily cell) $ toList board where
    cell = getElem x y board


-- | Get the cell above the position if the cell is in the same region
getUpperRelative :: Matrix Cell -> Pos -> Maybe Cell
getUpperRelative board (x, y) = do
    if x == 1 then Nothing else
        if isFamily (getElem (x - 1) y board) (getElem x y board)
            then Just (getElem (x - 1) y board)
            else Nothing


-- | Get the cell below the position if the cell is in the same region
getLowerRelative :: Matrix Cell -> Pos -> Maybe Cell
getLowerRelative board (x, y) = do
    if x == nrows board then Nothing else
        if isFamily (getElem (x + 1) y board) (getElem x y board)
            then Just (getElem (x + 1) y board)
            else Nothing

-- | Finds the possible values for a cell
--
-- The possible values for a cell are the values that are not already in the
-- same region nor in the neighbouring values of the cell. Also, the value of a
-- cell above another of the same region has to be greater than the value of the
-- cell below. Finally, the range of the values for a cell is 1 to the number of
-- cells in the region
getPossibleValues :: Matrix Cell -> Pos -> [CellValue]
getPossibleValues board (x, y) =
    reverse ([start+1..end-1] \\ nub (map value family ++ getNeighbors board (x, y)))
    where
        family = getFamily board (x,y)
        start = maybe 0 value (getLowerRelative board (x, y))
        end = maybe (length family + 1) value (getUpperRelative board (x, y))


-- | Returns the cells that are vertical at the top of a perfectly vertical
-- family
getFreeVertical :: Matrix Cell -> Maybe Cell -> Pos -> [Cell]
getFreeVertical board cell (x, y)
    | isNothing cell = []
    | definitive (fromJust cell) = getFreeVertical board (getLowerRelative board (x, y)) (x + 1, y)
    | otherwise = fromJust cell : getFreeVertical board (getLowerRelative board (x, y)) (x + 1, y)


getEmptyFamily :: Matrix Cell -> Pos -> [Cell]
getEmptyFamily board (x, y) = [cell | cell <- getFamily board (x,y), not (definitive cell)]
    where cell = getElem x y board

getAvailableValues :: Matrix Cell -> Pos -> [CellValue]
getAvailableValues board (x, y) =  reverse ([1..length family] \\ map value family)
    where
        family = getFamily board (x,y)

-- | Returns true if the cell is vertical at the top of a perfectly vertical
-- family
isVertical:: Matrix Cell -> Pos -> Bool
isVertical board (x, y) = do
    let emptyFamily = getEmptyFamily board (x, y)
    let verticalFamily = getFreeVertical board (safeGet x y board) (x, y)
    length emptyFamily == length verticalFamily


fillVerticals :: Matrix Cell -> Pos -> Matrix Cell
fillVerticals board (x,y)
    | x == ncols board && y == nrows board = board
    | definitive cell = fillVerticals board (xPos, yPos + 1)
    | isVertical board (xPos, yPos) = fillVerticals (setVerticalCells board (xPos, yPos) (getAvailableValues board (xPos, yPos))) (xPos, yPos + 1)
    | otherwise = fillVerticals board (xPos, yPos + 1)
    where
        yPos = if y == ncols board + 1 then 1 else y
        xPos = x + (if yPos == 1 then 1 else 0)
        cell = getElem xPos yPos board
        possibleValues = getPossibleValues board (xPos, yPos)


setVerticalCells :: Matrix Cell -> Pos -> [CellValue] -> Matrix Cell
setVerticalCells board (x, y) values = do
        let newBoard = setDefinitiveCell board (x, y) (head values)
        if null values then
            board
        else
            if not $ definitive (getElem x y board) then
                setVerticalCells newBoard (x + 1, y) (tail values)
            else
                setVerticalCells board (x + 1, y) values



setDefinitiveCell :: Matrix Cell -> Pos -> CellValue -> Matrix Cell
setDefinitiveCell board (x, y) value = setElem (Cell value True (region (getElem x y board))) (x, y) board

-- | Fill the cells that have only one possible value
fillMandatory :: Matrix Cell -> Pos -> Matrix Cell
fillMandatory board (x, y)
    | x == ncols board && y == nrows board = board
    | definitive cell = fillMandatory board (xPos, yPos + 1)
    | length possibleValues == 1 = fillMandatory (setDefinitiveCell board (xPos, yPos) (head possibleValues)) (xPos, yPos + 1)
    | otherwise = fillMandatory board (xPos, yPos + 1)
    where
        yPos = if y == ncols board + 1 then 1 else y
        xPos = x + (if yPos == 1 then 1 else 0)
        cell = getElem xPos yPos board
        possibleValues = getPossibleValues board (xPos, yPos)


changedCells :: Matrix Cell -> Matrix Cell -> Int
changedCells board1 board2 = length $ list1 \\ list2 where
    list1 = map value $ toList board1
    list2 = map value $ toList board2

initialFill :: Matrix Cell -> Matrix Cell
initialFill board = do
    let newBoard1 = fillMandatory board (0, 1)
    let newBoard2 = fillVerticals newBoard1 (0, 1)
    if changedCells board newBoard1 == 0 && changedCells board newBoard2 == 0 then
        newBoard2
    else
        initialFill newBoard2

cellValueToString :: CellValue -> String
cellValueToString 0 = " "
cellValueToString x = Prelude.show x

instance Show Cell where
    show cell = cellValueToString (value cell)

printBoard :: Matrix Cell -> IO ()
printBoard board = do
    putStrLn $ intercalate "\n" $ map (intercalate " ") $ toLists $ fmap show board
    putStrLn ""