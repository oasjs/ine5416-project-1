module Board (
    CellValue,
    CellRegion,
    Pos,
    Cell(..),
    makeBoard,
    fillMandatory,
    printBoard
) where

import Data.List
import Data.Matrix
import Data.Maybe

type CellValue = Int -- Used to represent the value of a cell
type CellRegion = Int -- Used to represent the region of a cell
type Pos = (Int, Int)

data Cell = Cell {
    value :: CellValue
    , definitive :: Bool
    , region :: CellRegion
    , pos :: Pos
    }

newCell :: (CellValue, CellRegion) -> Cell
newCell (value, region) = Cell {
    value = value
    , definitive = value /= 0
    , region = region
    , pos = (0, 0)
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
getFamily :: Matrix Cell -> Cell -> [Cell]
getFamily board cell = filter (isFamily cell) $ toList board


-- | Get the cell above the position if the cell is in the same region
getUpperRelative :: Matrix Cell -> Pos -> Maybe CellValue
getUpperRelative board (x, y) = do
    case safeGet x (y - 1) board of
        Just upperCell ->
            if isFamily upperCell (getElem x y board)
                then Just (value upperCell)
                else Nothing
        Nothing -> Nothing


-- | Get the cell below the position if the cell is in the same region
getLowerRelative :: Matrix Cell -> Pos -> Maybe CellValue
getLowerRelative board (x, y) = do
    case safeGet x (y + 1) board of
        Just lowerCell ->
            if isFamily lowerCell (getElem x y board)
                then Just (value lowerCell)
                else Nothing
        Nothing -> Nothing


-- | Finds the possible values for a cell
--
-- The possible values for a cell are the values that are not already in the
-- same region nor in the neighbouring values of the cell. Also, the value of a
-- cell above another of the same region has to be greater than the value of the
-- cell below. Finally, the range of the values for a cell is 1 to the number of
-- cells in the region
getPossibleValues :: Matrix Cell -> Pos -> [CellValue]
getPossibleValues board (x, y) =
    [start+1..end-1] \\ nub (map value family ++ getNeighbors board (x, y))
    where
        cell = getElem x y board
        family = getFamily board cell
        start = fromMaybe 0 (getLowerRelative board (x,y))
        end = fromMaybe (length family + 1) (getUpperRelative board (x,y))


-- | Check if the cell is at the top of a free vertical line
-- 
-- If the x coordinate of every free cell of the family is the same, then the
-- cell is at the top of a free vertical line
{- 
isVertical :: Board -> Pos -> Int -> Int -> (Int, Int)
isVertical board (x, y) l m = (len, minimo) where
    (len, minimo) = if getDownNeighbor board (x, y) > 0
                    then (l, getDownNeighbor+1)
                    else isVertical board (x-1, y) (l+1) 0 
-}

{-
iterateBoard :: (Board, Int, Int) -> ()
iterateBoard (board, ax, ay) | ay < length board = iterateBoard (board, (ax + 1) `mod` length board, ay + (if ax == (length board - 1) then 1 else 0))
                             | ay == (length board - 1) = ()
 -}

setDefinitiveCell :: Matrix Cell -> Pos -> CellValue -> Matrix Cell
setDefinitiveCell board (x, y) value = setElem (Cell value True (region (board ! (x, y))) (x, y)) (x, y) board

-- | Fill the cells that have only one possible value
fillMandatory :: Matrix Cell -> Pos -> Matrix Cell
fillMandatory board (x, y)
    | x <= 0 || y <= 0 = undefined
    | x == ncols board && y == nrows board = board
    | definitive cell = fillMandatory board (xPos + 1, yPos)
    | length possibleValues == 1 = fillMandatory (setDefinitiveCell board (xPos, yPos) (head possibleValues)) (xPos + 1, yPos)
    | otherwise = fillMandatory board (xPos + 1, yPos)
    where
        xPos = if x == ncols board + 1 then 1 else x
        yPos = y + (if xPos == 1 then 1 else 0)
        cell = getElem xPos yPos board
        possibleValues = getPossibleValues board (xPos, yPos)


cellValueToString :: CellValue -> String
cellValueToString 0 = " "
cellValueToString x = Prelude.show x

instance Show Cell where
    show cell = cellValueToString (value cell)

printBoard :: Matrix Cell -> IO ()
printBoard board = do
    putStrLn $ intercalate "\n" $ map (intercalate " ") $ toLists $ fmap show board
    putStrLn ""