module Main where

import NHL.Games
main :: IO ()
main = do
    games <- findGames
    print games
