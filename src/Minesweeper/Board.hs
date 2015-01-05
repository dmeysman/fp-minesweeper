-- | Operations on a minesweeper 'Board'.
module Minesweeper.Board
  ( Board
  -- * Creation
  , randomBoard
  -- * Modification
  , flagCell
  , clickCell
  -- * Validation
  , lost
  , won
  ) where

import qualified Data.Foldable    as Df
import qualified Data.List        as Dl
import qualified Data.Sequence    as Ds
import qualified System.Random    as Sr

import           Minesweeper.Cell

-- | A minesweeper 'Board'.
data Board = Board { width  :: Int
                   , height :: Int
                   , cells  :: Ds.Seq (Ds.Seq Cell)
                   }

instance Show Board where

  show b@(Board { width = n, cells = css })
    | lost b || won b = showUnmasked css
    | otherwise       = showMasked css
    where
      showUnmasked    = showMasked . fmap (fmap unmask)
      showMasked css' = rowSeparator ++ (Dl.intercalate rowSeparator (Df.toList $ fmap showRow css')) ++ rowSeparator
      showRow r       = '|' : (Dl.intercalate "|" . Df.toList . fmap show $ r) ++ "|\n"
      rowSeparator    = (Dl.intersperse '-' . take (succ n) $ repeat '+') ++ "\n"

-- | Generate a random minesweeper 'Board'.
randomBoard :: Int -> (Int, Int) -> (Int, Int) -> Board
randomBoard s d@(n, m) c@(cn, cm) = buildBoard d c . Dl.nub . take numberOfMines $ positions
  where
    positions     = [(n', m') | (n', m') <- zip (randoms s $ pred n) (randoms (succ s) $ pred m), (n', m') /= (cn, cm)]
    randoms s b   = Sr.randomRs (0, b) $ Sr.mkStdGen s
    numberOfMines = round $ fromIntegral (n * m) * density
    density       = 0.25

-- | Generate a minesweeper 'Board', from its dimensions, a first click and a list of positions for mines.
buildBoard :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Board
buildBoard (n, m) c = clickCell c . buildBoard'
  where
    buildBoard' (p : ps)                = adjustCell mine p . notifyNeighbours (neighbours p) $ buildBoard' ps
    buildBoard' _                       = emptyBoard
    notifyNeighbours (neighbour : ns) b = adjustCell (updateAdjacentMines succ) neighbour . notifyNeighbours ns $ b
    notifyNeighbours _                b = b
    mine                                = const minedCell
    emptyBoard                          = Board n m . Ds.fromList . fmap Ds.fromList . replicate m $ replicate n emptyCell

-- | Try to read the 'Cell' at the given position on the 'Board'.
readCell :: (Int, Int) -> Board -> Maybe Cell
readCell (n', m') Board { width = n , height = m, cells = css }
  | and [n' >= 0, n' < n, m' >= 0, m' < m] = Just $ Ds.index (Ds.index css n') m'
  | otherwise                              = Nothing

-- | Update the 'Cell' at the given position on the 'Board', failing silently.
adjustCell :: (Cell -> Cell) -> (Int, Int) -> Board -> Board
adjustCell f (n', m') (Board n m css) = let
                                          css' = Ds.adjust (\cs -> Ds.adjust f m' cs) n' css
                                        in
                                          Board n m css'

-- | Flag the 'Cell' at the given position on the 'Board', only if it is masked.
flagCell :: (Int, Int) -> Board -> Board
flagCell = adjustCell flag

-- | Click the 'Cell' at the given position on the 'Board', only if it is masked.
clickCell :: (Int, Int) -> Board -> Board
clickCell p b = clickCell' . fmap mined $ readCell p b
  where
    clickCell' (Just False) = adjustCell click p . propagatedClicks (neighbours p) $ b
    clickCell' _            = adjustCell click p b

-- | Propagate a click to a list of target positions.
propagatedClicks :: [(Int, Int)] -> Board -> Board
propagatedClicks []       b     = b
propagatedClicks (p : ps) b
  | notClickable $ readCell p b = propagatedClicks ps b
  | otherwise                   = adjustCell click p . propagatedClicks ps $ b
  where
    notClickable (Just c) = maybeMined c
    notClickable _        = True

-- | Get the list of neighbours of a position.
neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (n, m) = [(n', m') | n' <- [n - 1..n + 1], m' <- [m - 1..m + 1], (n', m') /= (n, m)]

-- | Check whether a 'Board' determines a lost game.
lost :: Board -> Bool
lost Board { cells = css } = Df.or $ fmap (Df.any causesLoss) css

-- | Check whether a 'Board' determines a won game.
won :: Board -> Bool
won Board { cells = css } = Df.and $ fmap (Df.all confirmsWin) css
