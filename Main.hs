
-- This is the haskell implementation of Tic Tac Toe Player Vs AI
-- Code help from : https://codereview.stackexchange.com/questions/110673/tic-tac-toe-in-haskell

module Main where

import System.IO (hFlush, stdout)
import Control.Exception (catch)
import Control.Monad(guard)
import Logic

-- the spaces are numbered 1-9 in the interface
spaceNumber :: Position -> Int
spaceNumber (y,x) = 3*y + x + 1

position :: Int -> Maybe Position
position n = do
  guard $ 1 <= n && n <= 9
  return $ (n - 1) `divMod` 3

boardString :: Board -> String
boardString board = concat rows
  where rows = zipWith rowString [0..2] board
        rowString y row = surround "\n" $ concat $ spaces y row
        spaces y row = zipWith (spaceString y) [0..2] row
        spaceString y x space = surround " " $ cellString (y,x) space
        cellString (y,x) space = case space of Nothing -> show (spaceNumber (y,x))
                                               (Just sym) -> show sym
        surround xs str = xs ++ str ++ xs

-- read position, checking it's valid and available
tryReadPosition :: Board -> IO (Maybe Position)
tryReadPosition board = (freePosition <$> readLn) `catch` returnNothing
  where freePosition n = do pos <- position n
                            if isFree pos board
                            then Just pos
                            else Nothing
        returnNothing = (const $ return Nothing) :: IOError -> IO (Maybe Position)

-- repeatedly show prompt and try to get valid input, until successful
prompt :: String -> IO (Maybe a) -> IO a
prompt message action = do
  putStr message >> hFlush stdout
  result <- action
  case result of (Just x) -> return x
                 Nothing  -> prompt message action

promptPosition :: GameState -> IO Position
promptPosition (player, board) = prompt ("Player " ++ show player ++ "'s turn: ") $
  tryReadPosition board

doTurn :: GameState -> IO GameState
doTurn state = do
  pos <- promptPosition state
  return $ makeTurn pos state

resultString :: GameResult -> String
resultString (Won player) = "Player " ++ show player ++ " is the winner!"
resultString Tie = "It's a tie!"

playGame :: IO ()
playGame = playGame' initialState
  where playGame' state@(player, board) = do putStrLn $ boardString board
                                             let result = gameResult board
                                               in case result of (Just r) -> putStrLn $ resultString r
                                                                 Nothing -> doTurn state >>= playGame'

-- This is to ask the player to restart the game - returns a boolean value
-- true for play again, false for no more game
promptPlayAgain :: IO Bool
promptPlayAgain = prompt "Play again (y/n)? " (yesOrNo <$> getLine)
  where yesOrNo "y" = Just True
        yesOrNo "n" = Just False
        yesOrNo _   = Nothing

main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe"
  playGame
  putStrLn ""
  again <- promptPlayAgain
  if again
  then main
  else putStrLn "Thank you!"