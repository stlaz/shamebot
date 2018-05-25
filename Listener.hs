module Listener where

import Data.ByteString.Char8 as BIN (ByteString, pack, unpack)
import Data.String (words)
import Data.List as List (isPrefixOf, delete)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map (empty, insert, lookup, delete, map)

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
import Common (BotState, CommandAction(SendMessage, AddToChat, DelFromChat,
               AddNamesToChat, RenamePerson, NOP), fst', trd')


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
            let actionList = pickActions msgs botname currState
            newState <- lift $ performActions sock actionList botname currState
            put newState
            lift $ print newState
        beginChatRoutine sock botname chans = do
            msgsSend sock $ serverLogin botname Nothing
            msgsSend sock $ joinChannels chans
        msgsSend _ [] = return 0
        msgsSend sock ((SendMessage m):msgs) = do
            send sock m
            msgsSend sock msgs


performActions _ [] _ state = return state
performActions sock (m:msgs) botname state =
    performAction sock m botname state >>= performActions sock msgs botname


-- TODO: merge the cases so that we don't depend on Message type but rather
--       perform actions based on pattern match of actions returned in list
--       from pickAction
performAction :: Socket -> CommandAction -> String -> BotState -> IO BotState
performAction _ NOP _ state = return state
performAction sock action@(SendMessage msg) botname state = do
    -- let actionList = pickAction msg botname state
    -- sendMsgsStateless sock $ if not (null actionList)
    --                             then actionList
    --                             else botMentionedCmd botname msg
    send sock msg
    return state
performAction sock action botname state = do
    let newState = changeBotState action botname state
    print newState
    return newState

changeBotState :: CommandAction -> String -> BotState -> BotState
changeBotState act@(AddNamesToChat chat names) botname state =
    Map.insert chat names state
changeBotState act@(AddToChat chat person) botname state =
    -- only add new chat if we're not joining ourselves
    if person /= botname then
            Map.insert chat (person:currNickList) state
        else
            state
    where
        currNickList = fromJust $ Map.lookup chat state
changeBotState act@(DelFromChat chat person) botname state =
    -- only del chat if we're not ones leaving
    if person /= botname then
        Map.insert chat (List.delete person currNickList) state
    else
        Map.delete chat state
    where
        -- TODO: remove code duplication
        currNickList = fromJust $ Map.lookup chat state
changeBotState act@(RenamePerson oldnick newnick) botname state =
    Map.map (\l -> newnick : List.delete oldnick l) state


botMentionedCmd botname (PrivMsg from _ chat _ msg)
    | isPrefixInList botname msg = [buildPrivMsg chat $
                            from ++ ": Do you require my service?"]
    | otherwise = []
    where
        isPrefixInList _ [] = False
        isPrefixInList a (s:ss)
            | a `isPrefixOfMaxPlus1Len` s = True
            | otherwise                   = isPrefixInList a ss
        -- FIXME: does not check the length, only takes!
        isPrefixOfMaxPlus1Len a b = isPrefixOf a $ take (length a + 1) b
botMentionedCmd botname _ = []


pickActions :: [Message] -> String -> BotState -> [CommandAction]
pickActions [] _ _ = []
pickActions (m:msgs) botname state =
    -- FIXME: will need to check whether every message got an action, if not,
    -- check botMentionedCmd (this is really stupid, I should fix it)
    -- FIXME: botMentionedCmd should become a regular command
    pickAction m botname state ++ pickActions msgs botname state


-- pickAction :: picks a function to handle the incoming message, executes it
--               and returns list of actions that need to be performed
pickAction :: Message -> String -> BotState -> [CommandAction]
pickAction msg@(PrivMsg _ _ _ _ []) botname state = []
pickAction msg@(PrivMsg _ _ _ _ msgwrds) botname state =
    pickCommand (head msgwrds) commandList state msg
pickAction msg@ChanMsg{} botname state = [pickChannelCommand msg]
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
