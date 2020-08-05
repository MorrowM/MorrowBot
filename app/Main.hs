{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

module Main where

import qualified Data.Text.IO as TIO

import Discord

import Handlers
import qualified Secrets

main :: IO ()
main = do
    userFacingError <- runDiscord $ def
        { discordToken = Secrets.tok
        , discordOnStart = execHandler onStart
        , discordOnEvent = flip $ execHandler . handleEvent
        }
    TIO.putStrLn userFacingError
