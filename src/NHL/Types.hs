{-# LANGUAGE OverloadedStrings #-}

module NHL.Types (
    Game
) where

import Data.Aeson

data Game
    = Played {
        awayName :: String,
        awayScore :: Int,
        homeName :: String,
        homeScore :: Int,
        date :: String }
    | NotPlayed {
        awayName :: String,
        homeName :: String,
        date :: String }
    deriving (Show)

instance FromJSON Game where
    parseJSON (Object v) = do
        awayName <- v .: "atn"
        awayScore <- v.: "ats"
        homeName <- v .: "htn"
        homeScore <- v .: "hts"
        date <- v .: "ts"
        rl <- v .: "rl"
        return $ case rl of
            False -> NotPlayed awayName homeName date
            True -> Played awayName (readInt awayScore) homeName (readInt homeScore) date

readInt "" = 0
readInt x  = read x
