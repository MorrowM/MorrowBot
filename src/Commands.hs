{-# LANGUAGE OverloadedStrings #-}

module Commands
  ( rootComm,
    Comm (..),
    WordsComm (..),
    ReactionWatchComm (..),
  )
where

import Data.Text (Text ())
import Database
import Options.Applicative
import Schema

data Comm = WordsComm WordsComm | ReactionWatchComm ReactionWatchComm

rootComm :: ParserInfo Comm
rootComm =
  info
    (rootSubComm <**> helper)
    ( fullDesc
        <> (progDesc "MorrowBot")
        <> header "Get notified!"
    )

rootSubComm :: Parser Comm
rootSubComm =
  hsubparser
    ( command "words" (info (WordsComm <$> wordsSubComm) (progDesc "Subscribe to notifications about watch words"))
        <> command "reactnotify" (info (ReactionWatchComm <$> reactionSubWatch) (progDesc "Subscribe to notifications about reactions"))
    )

wordsComm :: ParserInfo WordsComm
wordsComm =
  info
    (wordsSubComm <**> helper)
    ( fullDesc
        <> (progDesc "Watch word management")
        <> header "!words - manage your watch words"
    )

data WordsComm
  = Add [Text]
  | Remove [Text]
  | List
  | Clear

addOptions :: Parser WordsComm
addOptions = fmap Add $ some $ argument str (metavar "WORDS")

removeOptions :: Parser WordsComm
removeOptions = fmap Remove $ some $ argument str (metavar "WORDS")

wordsSubComm :: Parser WordsComm
wordsSubComm =
  hsubparser
    ( command "add" (info addOptions (progDesc "Add a watch word"))
        <> command "remove" (info removeOptions (progDesc "Remove a watch word"))
        <> command "list" (info (pure List) (progDesc "List all watch words"))
        <> command "clear" (info (pure Clear) (progDesc "Clear all watch words"))
    )

data ReactionWatchComm
  = Info
  | Self Bool
  | Msg Int Bool

reactionWatchComm :: ParserInfo ReactionWatchComm
reactionWatchComm =
  info
    (reactionSubWatch <**> helper)
    ( fullDesc
        <> (progDesc "Reaction subsciption management")
        <> header "!reactnotify - get notified about reactions"
    )

reactionSubWatch :: Parser ReactionWatchComm
reactionSubWatch =
  hsubparser
    ( command "info" (info (pure Info) (progDesc "Get info on your reaction subscriptions."))
        <> command "all" (info selfOptions (progDesc "Subscribe to all reactions on your messages."))
        <> command "message" (info msgOptions (progDesc "Subscribe to a specific message."))
    )

selfOptions :: Parser ReactionWatchComm
selfOptions =
  Self
    <$> switch
      ( long "remove"
          <> short 'r'
          <> help "Remove your subscription to this server."
      )

msgOptions :: Parser ReactionWatchComm
msgOptions =
  Msg
    <$> argument auto (metavar "MESSAGE_ID")
    <*> switch
      ( long "remove"
          <> short 'r'
          <> help "Remove this message from your subscriptions."
      )
