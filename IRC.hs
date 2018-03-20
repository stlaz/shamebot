module IRC where

import Data.List as List (isPrefixOf, delete, elem)
import Data.List.Split (splitOn)
import Data.ByteString.Char8 as BIN (ByteString, unpack, pack)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map (empty, insert, lookup, delete, map)

import Common (BotState, splitOnceBy, fst', snd')


data MsgType = NAMES | NAMES_END | JOIN | NICK | PART deriving Show

data Message = Unknown
    | PrivMsg {
        from      :: String,
        fromInfo  :: String,
        chat      :: String,
        msgRaw    :: String,
        msgWords  :: [String] }
    | ChanMsg {
        from    :: String,
        chat    :: String,
        msgtype :: MsgType,
        msgargs  :: [String] }
    | SrvMsg {
        msgtype   :: MsgType,
        msgarg    :: Maybe String,
        contRaw   :: String,
        contWords :: [String] }
    | Ping {from :: String}
    deriving Show

parseMsgs botname msgs =
        map (parseMsg botname) $ init $ splitOn "\r\n" $ unpack msgs

parseMsg botname msg
    | "PING" `isPrefixOf` msg =
        Ping{from=snd $ splitOnceBy ':' msg}
    | head msg == ':' =  privParsed
    | otherwise = Unknown
    where
        privParsed = case head $ tail msgWordString of
            "PRIVMSG" -> PrivMsg {
                    from      = fromString,
                    fromInfo  = snd nickSplit,
                    chat      = getSendtoChat,
                    msgRaw    = snd parseBoth,
                    msgWords  = words $ snd parseBoth
                }
            "JOIN" -> ChanMsg {
                    from      = fromString,
                    -- some servers prepend JOIN argument with ':' in which
                    -- case the joined channel appears in `snd parseBoth`
                    chat      = if length (fst parseBoth) == 1
                                    then snd parseBoth
                                    else chatStringPriv,
                    msgtype   = JOIN,
                    msgargs    = []
                }
            "NICK" -> ChanMsg {
                    from      = fromString,
                    chat      = "",
                    msgtype   = NICK,
                    msgargs    = [snd parseBoth]
                }
            "PART" -> ChanMsg {
                    from     = fromString,
                    chat      = chatStringPriv,
                    msgtype   = PART,
                    msgargs    = []
                }
            "353" -> ChanMsg {               -- RPL_NAMES
                    from      = fromString,
                    chat      = chatStringNames,
                    msgtype   = NAMES,
                    msgargs    = words $ snd parseBoth
                }
            "366" -> ChanMsg {               -- RPL_ENDOFNAMES
                    from      = fromString,
                    chat      = chatStringPriv,
                    msgtype   = NAMES_END,
                    msgargs    = []
                }
            _         -> Unknown
        getSendtoChat = if chatStringPriv == botname then fromString
                                                     else chatStringPriv
        fromString = fst nickSplit
        chatStringPriv = fst parseBoth !! 1
        chatStringNames  = fst parseBoth !! 3
        nickSplit = splitOnceBy '!' $ tail $ head msgWordString
        parseBoth = (words $ fst afterSplit, snd afterSplit)
        afterSplit = splitOnceBy ':' $ snd $ splitOnceBy ' ' msg
        msgWordString = words msg


stateChange msg@(ChanMsg from chat t args) botname state =
        case t of
            NAMES -> return $ Map.insert chat args state
            JOIN  -> if from /= botname then
                         return $ Map.insert chat (from:currNickList) state
                     else
                         return state
            NICK  -> return $ Map.map (\l -> args ++ List.delete from l) state
            PART  -> if from /= botname then
                         return $ Map.insert chat (List.delete from currNickList) state
                    else
                        return $ Map.delete chat state
            _     -> return state
    where
        currNickList = fromJust $ Map.lookup chat state


serverLogin botname Nothing =
    pack ("USER " ++ botname ++ " * 8 :a sad sad bot\r\n") :
        [pack $ "NICK " ++ botname ++ "\r\n"]
serverLogin botname passwd =
    pack ("PASS"  ++ fromJust passwd ++ "\r\n") : serverLogin botname Nothing


joinChannels = map (\x -> pack $ "JOIN " ++ x ++ "\r\n")

pong (Ping host) = [pack $ "PONG :" ++ host ++ "\r\n"]


shameCmd state (PrivMsg from _ chat _ msg)
    | length msg == 2 =
        if List.elem (msg !! 1) $ fromJust $ Map.lookup chat state
            then
                [buildActionMsg chat $ "publicly pronounces " ++ (msg !! 1) ++
                 " to be a dick"]
            else
               [buildPrivMsg chat $ "There's no such person as " ++ (msg !! 1)
                ++ ". Aren't you, " ++ from ++ ", the dick here?"]
    | otherwise = [buildPrivMsg chat "I don't know who to shame!"]
joinCmd state (PrivMsg _ _ chat _ msg)
    | length msg == 2 = [pack $ "JOIN " ++ (msg !! 1) ++ "\r\n"]
    | otherwise = [buildPrivMsg chat "Please specify a channel to join."]
leaveCmd state (PrivMsg _ _ chat _ _) = [pack $ "PART " ++ chat ++ "\r\n"]
commandsCmd state (PrivMsg _ _ chat _ _) =
        buildPrivMsg chat "Try my awesome commands:" : getCommandList
    where
        getCommandList = map
                    (\x ->
                        buildPrivMsg chat ("    " ++ fst' x ++ "    " ++ snd' x)
                    )
                    commandList

buildPrivMsg chat msg =
        pack $ "PRIVMSG " ++ chat ++ " :" ++ msg ++ "\r\n"
buildActionMsg chat msg =
        buildPrivMsg chat $ "\SOHACTION " ++ msg ++ "\SOH"

commandList :: [(String, String, BotState -> Message -> [ByteString])]
commandList = [
        ("!shame",    "Shame a person!",                              shameCmd),
        ("!join",     "Join a channel.",                              joinCmd),
        ("!leave",    "Leave current channel.",                       leaveCmd),
        ("!commands", "Shows this very helpful commands description", commandsCmd)
    ]
