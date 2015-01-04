-- | Operations on a minesweeper 'Minesweeper.Board' 'Cell'.
module Minesweeper.Cell
  ( Cell
  -- * Creation
  , emptyCell
  , minedCell
  -- * Modification
  , flag
  , click
  , unmask
  , updateAdjacentMines
  -- * Verification
  , mined
  , maybeMined
  , causesLoss
  , confirmsWin
  ) where

data State
  = Masked
  | Flagged
  | Clicked

data Contents
  = Mine
  | Empty

-- | A 'Cell' on a minesweeper 'Minesweeper.Board'.
data Cell = Cell State Contents Int

instance Show Cell where

  show (Cell Clicked Empty n) = show n
  show (Cell Clicked Mine  _) = "*"
  show (Cell Flagged _     _) = "F"
  show _                      = " "

-- | Create a masked 'Cell' that does not contain a mine.
emptyCell :: Cell
emptyCell = Cell Masked Empty 0

-- | Create a masked 'Cell' that contains a mine.
minedCell :: Cell
minedCell = Cell Masked Mine 0

-- | Flag a 'Cell', if it is masked.
flag :: Cell -> Cell
flag (Cell Masked c n) = Cell Flagged c n
flag c                 = c

-- | Click a 'Cell', if it is masked.
click :: Cell -> Cell
click (Cell Masked c n) = Cell Clicked c n
click c                 = c

-- | Forcefully click a 'Cell'.
unmask :: Cell -> Cell
unmask (Cell _ c n) = Cell Clicked c n

-- | Update the number of adjacent 'Cell's containing a mine.
updateAdjacentMines :: (Int -> Int) -> Cell -> Cell
updateAdjacentMines f (Cell s b n) = Cell s b $ f n

-- | Check whether a 'Cell' contains a mine.
mined :: Cell -> Bool
mined (Cell _ Mine _) = True
mined _               = False

-- | Check whether a 'Cell' contains a mine or is marked as such.
maybeMined :: Cell -> Bool
maybeMined (Cell Flagged _ _) = True
maybeMined c                  = mined c

-- | Check if a 'Cell' causes a loss.
causesLoss :: Cell -> Bool
causesLoss (Cell Clicked Mine _) = True
causesLoss _                     = False

-- | Check if a 'Cell' confirms a win.
confirmsWin :: Cell -> Bool
confirmsWin (Cell Flagged Mine  _) = True
confirmsWin (Cell Clicked Empty _) = True
confirmsWin _                      = False