{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DeriveGeneric              #-}
module Schema where

import Database.Persist (Entity(..), Entity)
import Database.Persist.TH
import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  WatchWord
    user Int
    guild Int
    text Text

    UserGuildText user guild text

  ReactionMessageWatch
    user Int
    message Int

    UserGuildMessage user message
  
  ReactionGuildWatch
    user Int
    guild Int

    UserGuild user guild
|]