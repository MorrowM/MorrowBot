# MorrowBot

## Installation

Requires the Haskell `stack` build tool: https:/haskellstack.org/

Add a file to `app/` called `Secrets.hs` with the following text:
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Secrets (tok) where

import Data.Text (Text ())

tok :: Text
tok = "<Bot token>"
```
At the project root run the command `stack run`. The project and its dependencies will be compiled and the bot will be run.

## Syntax

All commands begin with `!!`. Run `!!help` in Discord to get started.