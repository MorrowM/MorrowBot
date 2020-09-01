-- allows "string literals" to be Text
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import System.Environment

import Handlers

main :: IO ()
main = do
  tok <- T.pack <$> getEnv "MORROWBOT_TOK"

  userFacingError <-
    runDiscord $
      def
        { discordToken = tok,
          discordOnStart = execHandler onStart,
          discordOnEvent = flip $ execHandler . handleEvent
        }
  TIO.putStrLn userFacingError
