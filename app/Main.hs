{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           System.Environment
import           System.IO

import           Control.Concurrent.STM
import qualified Data.Text                 as T
import qualified EuphApi                   as E
import qualified EuphApi.Utils.Listing     as EL
import qualified System.Log.Formatter      as LF
import qualified System.Log.Handler        as LH
import qualified System.Log.Handler.Simple as LH
import qualified System.Log.Logger         as L

import           InfoBot

type BotSpecific        = ()
type ConnectionSpecific = TVar EL.Listing
type Bot     = E.Bot       BotSpecific ConnectionSpecific
type Config  = E.BotConfig BotSpecific ConnectionSpecific
type Command = E.Command   BotSpecific ConnectionSpecific

{-
 - Commands
 -}

recountCommand :: Command
recountCommand = E.specificCommand "recount" $ \msg -> do
  lVar <- E.getConnectionInfo
  myID <- E.sessSessionID <$> E.getOwnView
  list <- E.who
  let l = EL.remove myID $ EL.fromList list
  liftIO $ atomically $ writeTVar lVar l
  void $ E.nick $ nameFromListing l
  void $ E.replyTo msg "Recalibrated!"

myHelp :: T.Text -> T.Text
myHelp name =
  "Displays information about the clients in a room in its nick:\n\
  \(<people>P <bots>B <lurkers>L <bot-lurkers>N)\n\
  \!recount " <> E.atMention name <> " - recalibrates the bot\n\n\
  \Created by @Garmy using https://github.com/Garmelon/EuphApi.\n"

myCommands :: [Command]
myCommands =
  [ E.pingCommand "Pong!"
  , E.generalPingCommand "Pong!"
  , E.helpCommand myHelp
  , E.generalHelpCommand (const "I show how many people, bots, lurkers etc. are online.")
  , E.uptimeCommand
  , E.generalUptimeCommand -- most bots don't do this
  , E.killCommand "Bye!"
  , E.restartCommand "brb"
  -- non-botrulez commands
  , recountCommand
  ]

{-
 - Handler
 -}

myBotHandler :: E.EventType -> Bot ()
myBotHandler (E.EuphEvent e) = do
  -- run commands
  E.runCommands myCommands e
  -- update listing
  lVar <- E.getConnectionInfo
  EL.update lVar e
  -- InfoBot logic
  handleEvent e
myBotHandler _ = return ()

handleEvent :: E.Event -> Bot ()
-- Set nick as soon as you have access to the room.
handleEvent (E.SnapshotEvent _ _ _ _) = updateNick
handleEvent (E.JoinEvent _)           = updateNick
handleEvent (E.PartEvent _)           = updateNick
handleEvent _                         = return ()

updateNick :: Bot ()
updateNick = do
  lVar <- E.getConnectionInfo
  l    <- liftIO $ atomically $ readTVar lVar
  void $ E.nick $ nameFromListing l

{-
 - Config
 -}

myBotConfig :: String -> Config
myBotConfig room = E.BotConfig
  { E.botAddress           = "euphoria.io"
  , E.botRoom              = room
  , E.botPassword          = Nothing
  , E.botNick              = "InfoBot"
  , E.botHandler           = myBotHandler
  , E.botInfo              = ()
  , E.botNewConnectionInfo = atomically $ newTVar EL.empty
  , E.botReconnectPolicy   = E.defaultReconnectPolicy
  }

main :: IO ()
main = do
  -- Set up logging with custom message style
  myHandler <- LH.verboseStreamHandler stdout L.INFO
  let myFormatter        = LF.simpleLogFormatter "<$time> [$loggername/$prio] $msg"
      myFormattedHandler = LH.setFormatter myHandler myFormatter
  L.updateGlobalLogger L.rootLoggerName (L.setHandlers [myFormattedHandler])
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.INFO)

  -- Use args to determine room and start the bot
  args <- getArgs
  case args of
    [room] -> E.runBot (return $ myBotConfig room)
    _      -> do
      name <- getProgName
      putStrLn "  USAGE:"
      putStr name
      putStrLn " <room>"
