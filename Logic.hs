
-- This is the haskell implementation of Tic Tac Toe Player Vs Computer
-- Code help from : https://codereview.stackexchange.com/questions/110673/tic-tac-toe-in-haskell

module Logic
( Symbol(..)
, Space
, Board
, Position
, GameResult(..)
, GameState
, placeSymbolAt
, isFree
, gameResult
, initialState
, makeTurn
) where

import Data.Maybe (isNothing, isJust)
import Data.List (find, transpose)
import Control.Monad (join, foldM, (>=>))

data Symbol = X | O deriving (Show, Eq)
type Space = Maybe Symbol
type Board = [[Space]] -- 3x3
type Position = (Int, Int) -- row,column
data GameResult = Won Symbol | Tie deriving (Show)

-- modify a list by applying a function on the nth element
mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt pos f list = zipWith compute [0..] list
  where compute i x
          | i == pos   = f x
          | otherwise = x

-- set nth element of a list
setAt :: Int -> a -> [a] -> [a]
setAt pos x = mapAt pos (const x)

placeSymbolAt :: Position -> Symbol -> Board -> Board
placeSymbolAt (y,x) sym = mapAt y (setAt x (Just sym))

at :: Position -> Board -> Space
at (y,x) board = board !! y !! x

isFree :: Position -> Board -> Bool
isFree pos = isNothing . at pos

isFull :: Board -> Bool
isFull = all (all isJust)

sequences :: Board -> [[Space]]
sequences board = rows ++ column ++ [mainDiagonal, secondaryDiagonal]
  where rows   = board
        column = transpose board
        mainDiagonal      = [at (x,x) board | x <- [0..2]]
        secondaryDiagonal = [at (x,2-x) board | x <- [0..2]] 

-- if sequence spaces are all Just sym, then Just sym. Otherwise, Nothing.
winnerOfSequence :: [Space] -> Maybe Symbol
winnerOfSequence = symbols >=> single
  where symbols = sequence :: [Space] -> Maybe [Symbol]
        single = foldM1 merge :: [Symbol] -> Maybe Symbol
        foldM1 f (x:xs) = foldM f x xs
        merge x y | x == y = Just x
                  | otherwise = Nothing

winner :: Board -> Maybe Symbol
winner = findJust . map winnerOfSequence . sequences
  where findJust = join . find isJust :: [Maybe Symbol] -> Maybe Symbol

gameResult :: Board -> Maybe GameResult
gameResult board = case (winner board, isFull board) of (Just x, _)      -> Just $ Won x
                                                        (Nothing, True)  -> Just Tie
                                                        (Nothing, False) -> Nothing

type GameState = (Symbol, Board) -- current player, board

initialState :: GameState
initialState = (X, emptyBoard)
  where emptyBoard = replicate 3 $ replicate 3 Nothing

makeTurn :: Position -> GameState -> GameState
makeTurn pos (player, board) = (next player, placeSymbolAt pos player board)
  where next X = O
        next O = X
