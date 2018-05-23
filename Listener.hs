module Listener where

import Data.ByteString.Char8 as BIN (ByteString, pack, unpack)
import Data.String (words)
import Data.List as List (isPrefixOf, delete)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map (empty, map)

import Control.Monad
import Control.Monad.State.Lazy

import Network (sClose)
import Network.Socket (
    Family(AF_INET, AF_INET6), SocketOption(ReuseAddr),
    AddrInfo(AddrInfo), Socket,
    addrAddress, addrFamily, addrSocketType, addrProtocol,
    socket, setSocketOption, connect, getAddrInfo
    )
import Network.Socket.ByteString (send, recv)

import IRC
import Common (BotState, CommandAction(SendMessage), fst', trd')


serverConnect :: String -> String -> Bool -> [String] -> String -> IO ()
serverConnect host port ipv4 channels botname = do
    addrList <- getAddrInfo Nothing (Just host) (Just port)
    let addr:_ = if ipv4 then filter isIPv4 addrList
                         else addrList
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging
    connect sock (addrAddress addr)
    beginChatRoutine sock botname channels
    execStateT (forever $ perform sock botname) Map.empty
    sClose sock -- TODO: close socket in interrupt handling
    where
        perform :: Socket -> String -> StateT BotState IO ()
        perform sock botname = do
            got <- lift $ recv sock 1024
            lift $ putStr $ unpack got
            let msgs = parseMsgs botname got
            lift $ print msgs
            currState <- get
            newState <- lift $ sendMsgs sock msgs botname currState
            put newState
            lift $ print newState
        beginChatRoutine sock botname chans = do
            sendMsgsStateless sock $ serverLogin botname Nothing
            sendMsgsStateless sock $ joinChannels chans


sendMsgs _ [] _ state = return state
sendMsgs sock (m:msgs) botname state =
    sendMsg sock m botname state >>= sendMsgs sock msgs botname


sendMsgsStateless sock [] = return 0
sendMsgsStateless sock (SendMessage m:msgs) = do
    send sock m
    sendMsgsStateless sock msgs


-- TODO: merge the cases so that we don't depend on Message type but rather
--       perform actions based on pattern match of actions returned in list
--       from pickAction
sendMsg :: Socket -> Message -> String -> BotState -> IO BotState
sendMsg sock msg@ChanMsg{} botname state = do
    newState <- stateChange msg botname state
    print newState
    return newState
sendMsg sock msg botname state = do
    -- FIXME: botMentionedCmd should become a regular command
    let actionList = pickAction msg botname state
    sendMsgsStateless sock $ if not (null actionList)
                                then actionList
                                else botMentionedCmd botname msg
    return state


botMentionedCmd botname (PrivMsg from _ chat _ msg)
    | isPrefixInList botname msg = [buildPrivMsg chat $
                            from ++ ": Do you require my service?"]
    | otherwise = []
    where
        isPrefixInList _ [] = False
        isPrefixInList a (s:ss)
            | a `isPrefixOfMaxPlus1Len` s = True
            | otherwise                   = isPrefixInList a ss
        -- FIXME: does not check the lenght, only takes!
        isPrefixOfMaxPlus1Len a b = isPrefixOf a $ take (length a + 1) b
botMentionedCmd botname _ = []


-- pickAction :: picks a function to handle the incoming message, executes it
--               and returns list of actions that need to be performed
pickAction msg@(PrivMsg _ _ _ _ []) botname state = []
pickAction msg@(PrivMsg _ _ _ _ msgwrds) botname state =
    pickCommand (head msgwrds) commandList state msg
pickAction msg@Ping{} botname state = pong msg
pickAction Unknown _ _ = []


pickCommand :: String
                -> [(String, String, BotState -> Message -> [CommandAction])]
                -> (BotState -> Message -> [CommandAction])
pickCommand _ [] = \x -> const []
pickCommand cmdName (c:cmds)
    | cmdName == fst' c = trd' c
    | otherwise         = pickCommand cmdName cmds


isIPv4 (AddrInfo _ AF_INET _ _ _ _) = True
isIPv4 _ = False
