{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           System.Environment
import           System.IO

import           Control.Concurrent.STM
import qualified EuphApi                    as E
import qualified EuphApi.Utils.DetailedHelp as E
import qualified EuphApi.Utils.Listing      as EL
import qualified System.Log.Formatter       as LF
import qualified System.Log.Handler         as LH
import qualified System.Log.Handler.Simple  as LH
import qualified System.Log.Logger          as L

import           InfoBot

type BotSpecific        = ()
type ConnectionSpecific = TVar EL.Listing
type Bot     = E.Bot       BotSpecific ConnectionSpecific
type Config  = E.BotConfig BotSpecific ConnectionSpecific
type Command = E.Command   BotSpecific ConnectionSpecific

{-
 - Commands
 -}

recountAction :: E.Message -> Bot ()
recountAction msg = do
  lVar <- E.getConnectionInfo
  myID <- E.sessSessionID <$> E.getOwnView
  list <- E.who
  let l = EL.remove myID $ EL.fromList list
  liftIO $ atomically $ writeTVar lVar l
  void $ E.nick $ nameFromListing l
  void $ E.replyTo msg "Recalibrated!"

myHelp :: Command
myHelp = E.helpCommand $ \n ->
  "Displays information about the clients in a room in its nick:\n\
  \(<people>P <bots>B <lurkers>L <bot-lurkers>N)\n\n\
  \!recount " <> E.atMention n <> " - Recount people in the room\n\n\
  \Created by @Garmy using EuphApi.\n\
  \For additional info, try \"!help " <> E.atMention n <> " <topic>\". Topics are:\n\
  \count, lurkers, changelog"


myDetailedHelp :: Command
myDetailedHelp = E.detailedHelpCommand
  [ ("count", \_ ->
      "This bot counts the number of clients connected to a room.\
      \ If you open a room in two different tabs, the bot counts you twice.\n\
      \The euphoria client, on the other hand, usually displays all connections\
      \ of an account as one nick in the nick list. \
      \ Because of that, this bot's count is always as high as, or higher than,\
      \ the number of nicks on the nick list, similar to the number on the button\
      \ to toggle the nick list.\n\n\
      \If the bot's count is off, try a !recount."
    )
  , ("lurkers", \_ ->
      "People or bots who are connected to the room but haven't chosen a nick are lurkers.\
      \ The euphoria client doesn't display them in the nick list.\n\
      \This bot differentiates between people (L) and bots (N) who are lurking."
    )
  , ("changelog", \_ ->
      "<2018-02-26> Port bot to EuphApi\n\
      \<2017-10-22> Add !recount command\n\
      \<2017-10-22> Fix bot counting incorrectly"
    )
  ]

myCommands :: [Command]
myCommands =
  [ E.pingCommand        "Pong!"
  , E.generalPingCommand "Pong!"
  , E.generalHelpCommand (const "I show how many people, bots, lurkers etc. are online.")
  , E.uptimeCommand
  , E.generalUptimeCommand
  , E.killCommand "Bye!"
  , E.restartCommand "brb"
  , myHelp
  , myDetailedHelp
  -- non-botrulez commands
  , E.command         "recount" recountAction
  , E.specificCommand "recount" recountAction
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
