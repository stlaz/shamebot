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
import Common (BotState, fst', trd')


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
sendMsgsStateless sock (m:msgs) = do
    send sock m
    sendMsgsStateless sock msgs


sendMsg :: Socket -> Message -> String -> BotState -> IO BotState
sendMsg sock msg@ChanMsg{} botname state = do
        newState <- stateChange msg botname state
        print newState
        return newState
sendMsg sock msg botname state = do
        sendMsgsStateless sock $ worker msg botname state
        return state


worker msg@PrivMsg{} botname state =
        if not (null actionRet) then actionRet
                                else botMentionedCmd msg
    where
        actionRet = pickAction state msg
        botMentionedCmd (PrivMsg from _ chat _ msg)
            | isPrefixInList botname msg = [buildPrivMsg chat $
                                    from ++ ": Do you require my service?"]
            | otherwise = []
        isPrefixInList _ [] = False
        isPrefixInList a (s:ss)
            | a `isPrefixOfMaxPlus1Len` s = True
            | otherwise                   = isPrefixInList a ss
        -- FIXME: does not check the lenght, only takes!
        isPrefixOfMaxPlus1Len a b = isPrefixOf a $ take (length a + 1) b

worker msg@Ping{} botname state = pong msg
worker Unknown _ _ = []


pickAction state privmsg@(PrivMsg _ _ _ _ msg) =
        pickCommand (head msg) commandList state privmsg


pickCommand :: String
                -> [(String, String, BotState -> Message -> [ByteString])]
                -> (BotState -> Message -> [ByteString])
pickCommand _ [] = \x -> const []
pickCommand cmdName (c:cmds)
    | cmdName == fst' c = trd' c
    | otherwise         = pickCommand cmdName cmds


isIPv4 (AddrInfo _ AF_INET _ _ _ _) = True
isIPv4 _ = False
