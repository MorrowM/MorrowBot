{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Handlers
    ( onStart 
    , handleEvent
    , execHandler
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Data.Bits
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import Options.Applicative.Help.Chunk
import System.Exit
import Text.Printf

import Discord
import Discord.Requests
import Discord.Types
import Discord.Internal.Rest.Prelude

import qualified Database.Persist as P
import qualified Database.Persist.Sql as SQL

import Commands
import Database
import Schema

import Debug.Trace

newtype HandlerT m a = HandlerT
    { runHandlerT :: ExceptT T.Text (ReaderT DiscordHandle m) a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans HandlerT where
    lift = HandlerT . lift . lift

getDis :: Monad m => HandlerT m DiscordHandle
getDis = HandlerT $ lift ask

raiseErr :: Monad m => T.Text -> HandlerT m a
raiseErr txt = HandlerT $ ExceptT $ return $ Left txt

catchErr :: Handler () -> Handler ()
catchErr h = do
    dis <- getDis
    eitherVal <- liftIO $ runReaderT (runExceptT $ runHandlerT h) dis
    case eitherVal of
        Left txt -> liftIO $ TIO.putStr txt
        Right val -> return val

assertTrue :: Bool -> Handler ()
assertTrue True = return ()
assertTrue False = raiseErr ""

whenJust :: Monad m => Maybe a -> HandlerT m a
whenJust ma = case ma of
    Nothing -> raiseErr "" -- Fail silently
    Just a -> return a

runDis  :: (FromJSON a, Request (r a)) => r a -> Handler a
runDis r = do
    dis <- getDis
    res <- liftIO $ restCall dis r
    case res of
        Left err -> raiseErr $ "RequestError: " `T.append` tshow err `T.append` "\n"
        Right val -> return val

type Handler a = HandlerT IO a

execHandler :: Handler a -> DiscordHandle -> IO ()
execHandler h dis = flip runReaderT dis $ do
    eErrVal <- runExceptT (runHandlerT h)
    case eErrVal of
        Left txt -> liftIO $ TIO.putStrLn txt
        Right _ -> return ()

runDB :: DatabaseAction a -> Handler a
runDB = liftIO . db

onStart :: Handler ()
onStart = catchErr $ do
    runDB $ SQL.runMigration migrateAll
    dis <- getDis
    cache <- liftIO $ readCache dis
    liftIO $ putStrLn $ printf "Connected as %s." (userName $ _currentUser cache)

handleEvent :: Event -> Handler ()
handleEvent event = catchErr $ case event of
    MessageCreate msg -> handleMessageCreate msg
    MessageReactionAdd rct -> handleMessageReactionAdd rct
    _ -> pure ()

handleMessageReactionAdd :: ReactionInfo -> Handler ()
handleMessageReactionAdd rct = catchErr $ do
    dis <- getDis
    cache <- liftIO $ readCache dis
    msg <- runDis $ GetChannelMessage (reactionChannelId rct, reactionMessageId rct)
    assertTrue $ reactionUserId  rct /= userId (_currentUser cache)
    guild <- whenJust (reactionGuildId rct)

    messageUsersToNotify <- (fmap $ map $ toEnum . reactionMessageWatchUser . P.entityVal) $
        runDB $ P.selectList [ReactionMessageWatchMessage P.==. fromEnum (reactionMessageId rct)] []
    guildUsersToNotify <- (fmap $ map $ toEnum . reactionGuildWatchUser . P.entityVal) $ 
        runDB $ P.selectList [ReactionGuildWatchGuild P.==. fromEnum guild, ReactionGuildWatchUser P.==. fromEnum (userId $ messageAuthor msg)] []
    
    let usersToNotify = messageUsersToNotify `union` guildUsersToNotify

    user <- runDis $ GetUser (reactionUserId rct)
    forM_ usersToNotify $ \usrId -> do
        dm <- runDis $ CreateDM usrId
        void $ runDis $ CreateMessage (channelId dm) $
            T.concat [userName user, " reacted to a subscribed message with ", emojiName (reactionEmoji rct)
                , "!\n> ", clip 300 (messageText msg), "\n", messageUrl msg guild]

handleMessageCreate :: Message -> Handler ()
handleMessageCreate msg = catchErr $ do
    handleComm msg
    handleWatchWords msg
    

handleWatchWords :: Message -> Handler ()
handleWatchWords msg = catchErr $ do
    dis <- getDis
    cache <- liftIO $ readCache dis
    when (userId (_currentUser cache) == userId (messageAuthor msg)) $
        raiseErr ""
    guild <- whenJust (messageGuild msg)
    wrds <- fmap (map P.entityVal) $ runDB $ P.selectList [WatchWordGuild P.==. (fromEnum guild)] []
    channel <- runDis $ GetChannel (messageChannel msg)
    guildObj <- runDis $ GetGuild guild

    let wrdMatches = filter ((`T.isInfixOf` (T.toCaseFold $ messageText msg)) . T.toCaseFold . getText) wrds
        usersToPing = nub $ map (toEnum . watchWordUser) wrdMatches

    forM_ usersToPing $ \usr -> do
        mem <- runDis $ GetGuildMember guild usr
        when (usr /= userId (messageAuthor msg) && canView guildObj channel mem) $ do
            dm <- runDis $ CreateDM usr
            void $ runDis $ CreateMessage (channelId dm) $
                userName (messageAuthor msg) `T.append` " mentioned a watch word in their message:\n> "
                `T.append` clip 300 (messageText msg) `T.append` "\n" `T.append` messageUrl msg guild

handleComm :: Message -> Handler ()
handleComm msg = catchErr $ 
    if "!!help" `T.isPrefixOf` (messageText msg) then
        runComm ["-h"] msg
    else if "!!" `T.isPrefixOf` (messageText msg) then
        let args = map T.unpack $ wordsWithQuotes (T.drop 2 (messageText msg))
        in runComm args msg
    else pure ()

runComm :: [String] -> Message -> Handler ()
runComm args msg = catchErr $ case execParserPure defaultPrefs rootComm args of

    Success whichComm -> do
        reactPositive
        case whichComm of 
            WordsComm comm -> runWordsComm msg comm
            ReactionWatchComm comm -> runReactionWatchComm msg comm

    Failure f -> do
        let (help, status, _) = execFailure f ""
            helpStr = "```" ++ show help ++ "```"

        if status == ExitSuccess
            then reactPositive
            else reactNegative
        
        void $ runDis $ CreateMessage (messageChannel msg) (T.pack helpStr)
    where
        reactPositive = runDis $ CreateReaction (messageChannel msg, messageId msg) ":white_check_mark:"
        reactNegative = runDis $ CreateReaction (messageChannel msg, messageId msg) ":x:"

runWordsComm :: Message -> WordsComm -> Handler ()
runWordsComm msg wordsComm = catchErr $ case wordsComm of
    Add wrds -> case messageGuild msg of
        Nothing -> do
            send "This command can only be run in a server."
        Just guild -> do
            existing <- runDB $ P.selectList [WatchWordUser P.==. (fromEnum $ userId $ messageAuthor msg), WatchWordGuild P.==. (fromEnum guild)] []
            if length wrds + length existing > 80
            then send "Cannot add that many watch words." 
            else do
                failures <- forM wrds $ \wrd -> do
                    if T.length wrd > 20 || T.length wrd < 1
                        then return False
                        else do
                            res <- runDB $ P.insertBy $ WatchWord (fromEnum $ userId $ messageAuthor msg) (fromEnum guild) wrd
                            return $ isLeft res
                if or failures
                then send "One or more words aready could not be added."
                else send "Word(s) added successfully."
    Remove wrds -> case messageGuild msg of
        Nothing -> do
            send "This command can only be run in a server."
        Just guild -> do
            failures <- forM wrds $ \wrd -> do
                    let record = UserGuildText (fromEnum $ userId $ messageAuthor msg) (fromEnum guild) wrd
                    res <- runDB $ P.getBy record
                    runDB $ P.deleteBy record
                    return $ isNothing res
            if or failures
            then send "One or more words do not exist in your word list."
            else send "Word(s) removed successfully."
    List -> case messageGuild msg of
        Nothing -> do
            wrds <- runDB $ P.selectList [WatchWordUser P.==. (fromEnum $ userId $ messageAuthor msg)] [P.Asc WatchWordText]
            send $ "Your watch words are:\n" <> T.intercalate ", " (getText . P.entityVal <$>  wrds)
        Just guild -> do
            wrds <- runDB $ P.selectList [WatchWordUser P.==. (fromEnum $ userId $ messageAuthor msg), WatchWordGuild P.==. (fromEnum guild)] [P.Asc WatchWordText]
            case wrds of
                [] -> send $ "You have no watch words."
                xs -> send $ "Your watch words are:\n" <> T.intercalate ", " (getText . P.entityVal <$> xs)
    Clear -> case messageGuild msg of
        Nothing -> do
            runDB $ P.deleteWhere [WatchWordUser P.==. (fromEnum $ userId $ messageAuthor msg)]
            send $ "All words cleared."
        Just guild -> do
            runDB $ P.deleteWhere [WatchWordUser P.==. (fromEnum $ userId $ messageAuthor msg), WatchWordGuild P.==. (fromEnum guild)]
            send $ "All words cleared."
    where
        send :: T.Text -> Handler ()
        send = void . runDis . CreateMessage (messageChannel msg)

runReactionWatchComm :: Message -> ReactionWatchComm -> Handler ()
runReactionWatchComm msg comm = catchErr $ case comm of
    Info -> do
        case messageGuild msg of
            Nothing -> send "This command can only be run inside a server."
            Just guild -> do
                mGuildSub <- runDB $ P.getBy (UserGuild (fromEnum $ userId $ messageAuthor msg) (fromEnum guild))
                case mGuildSub of
                    Nothing ->
                        send "Serverwide reactions are turned off."
                    Just _ -> send "Serverwide reactions are turned on."
                
                mMsgSubs <- fmap (map P.entityVal) $ runDB $ P.selectList [ReactionMessageWatchUser P.==. (fromEnum $ userId $ messageAuthor msg)] []
                case mMsgSubs of
                    [] -> send "You are not subscribed to reactions on any specific messages."
                    msgSubs -> send $ "You are subscribed to the following messages:\n" `T.append` 
                        T.intercalate ", " (tshow . reactionMessageWatchMessage <$> mMsgSubs)
    Self disable -> if disable
        then case messageGuild msg of
            Nothing -> do
                runDB $ P.deleteWhere [ReactionGuildWatchUser P.==. (fromEnum $ userId $ messageAuthor msg)]
                send $ "All subscriptions removed."
            Just guild -> do
                runDB $ P.deleteWhere [ReactionGuildWatchUser P.==. (fromEnum $ userId $ messageAuthor msg), ReactionGuildWatchGuild P.==. (fromEnum guild)]
                send $ "Subscription to this server removed.."
        else case messageGuild msg of
            Nothing -> send "This command can only be run inside a server."
            Just guild -> do
                res <- runDB $ P.insertBy $ ReactionGuildWatch (fromEnum $ userId $ messageAuthor msg) (fromEnum guild)
                if isLeft res
                then send "You are already subscribed to reactions in this server."
                else send "Successfully subscribed to reactions in this server."

    Msg watchMsg disable -> if disable
        then do
            runDB $ P.deleteWhere [ReactionMessageWatchUser P.==. (fromEnum $ userId $ messageAuthor msg), ReactionMessageWatchMessage P.==. watchMsg]
            send $ "Subscription to this message removed."
        else do
            existing <- runDB $ P.selectList [ReactionMessageWatchUser P.==. (fromEnum $ userId $ messageAuthor msg)] []
            if length existing > 30
            then send "Sorry, you cannot subscibe to any more messages."
            else do
                res <- runDB $ P.insertBy $ ReactionMessageWatch (fromEnum $ userId $ messageAuthor msg) watchMsg
                if isLeft res
                then send "You are already subscribed to this message"
                else send "Successfully subscribed to the message."
        
    where
        send = void . runDis . CreateMessage (messageChannel msg)

getText :: WatchWord -> T.Text
getText (WatchWord _ _ txt) = txt

messageUrl :: Message -> GuildId -> T.Text
messageUrl msg guild = T.concat ["https://discord.com/channels/", tshow guild, "/", tshow (messageChannel msg), "/", tshow (messageId msg)]

tshow :: Show a => a -> T.Text
tshow = T.pack . show

clip :: Int -> T.Text -> T.Text
clip n txt = if T.length txt <= n 
    then txt
    else T.take n txt `T.append` "..."

wordsWithQuotes :: T.Text -> [T.Text]
wordsWithQuotes = concat . wordsEveryOther . T.splitOn "\""
    where
        wordsEveryOther :: [T.Text] -> [[T.Text]]
        wordsEveryOther [] = []
        wordsEveryOther [z] = [T.words z]
        wordsEveryOther (x:y:xs) = T.words x : [y] : wordsEveryOther xs

computeBasePerms :: GuildMember -> Guild -> Integer
computeBasePerms mem guild =
    if guildOwnerId guild == userId (memberUser mem)
    then allPerms
    else
        let everyonePerms = case listToMaybe $ filter ((==guildId guild) . roleId) $ guildRoles guild of
                Nothing -> error "A guild with no base perms???"
                Just everyoneRole -> rolePerms everyoneRole
            memRoles = filter ((`elem` memberRoles mem) . roleId) (guildRoles guild)
            memPerms = foldl (.|.) everyonePerms (rolePerms <$> memRoles) -- TODO figure out how admin works
        in memPerms
    where allPerms = 0xFFFFFFFF

computeOverwrites :: Integer -> GuildMember -> Channel -> Integer
computeOverwrites basePerms mem cha =
    let overwrites = channelPermissions cha
        mEveryoneOver = listToMaybe $ filter ((==channelId cha) . overwriteId) overwrites
        roleOvers = filter ((`elem` memberRoles mem) . overwriteId) overwrites
        mMemOver = listToMaybe $ filter ((==userId (memberUser mem)) . overwriteId) overwrites
        permsEveryone = case mEveryoneOver of
            Nothing -> basePerms
            Just everyoneOver -> basePerms .&. complement (overwriteDeny everyoneOver) .|. overwriteAllow everyoneOver
        overAllow = bitwiseOr 0 (overwriteAllow <$> roleOvers)
        overDeny  = bitwiseOr 0 (overwriteDeny <$> roleOvers)
        permsRoles = permsEveryone .&. complement overDeny .|. overAllow
        perms = case mMemOver of
            Nothing -> permsRoles
            Just memOver -> basePerms .&. complement (overwriteDeny memOver) .|. overwriteAllow memOver
    in perms
    
    where
        bitwiseOr = foldl (.|.)

computePerms :: Guild -> Channel -> GuildMember -> Integer
computePerms guild cha mem =
    let basePerms = computeBasePerms mem guild
    in computeOverwrites basePerms mem cha

canView :: Guild -> Channel -> GuildMember -> Bool
canView guild cha mem = (/=0) $ (.&. 0x00000400) $ f $ computePerms guild cha mem
    where f x = traceShow x x