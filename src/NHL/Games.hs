{-# LANGUAGE OverloadedStrings #-}

module NHL.Games (
    findGames
) where

import Control.Lens ((^.))
import Network.Wreq (responseBody, get)

import Text.Parsec as P
import Text.Parsec.ByteString.Lazy

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B (pack)

import Data.Aeson
import Data.Aeson.Types as A

import NHL.Types

jsonStringP = do
    string "loadScoreboard("
    s <- manyTill anyChar (char ')' >> eof)
    return $ s

getJavascript = do
    response <- get "http://live.nhle.com/GameData/RegularSeasonScoreboardv3.jsonp"
    return $ response ^. responseBody

findGames :: IO ([Game])
findGames = do
    js <- getJavascript
    case (P.parse jsonStringP "" js) of
        Left e -> return []
        Right r -> do
            l <- return $ getGames (B.pack r)
            case l of
                Nothing -> return []
                (Just gl) -> return gl
    where
        getGames bs = do
            x <- decode bs
            parseMaybe games x
        games x = x .: "games"
