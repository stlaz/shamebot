module Common where

import Data.List (findIndex, isPrefixOf, tails)
import Data.Map.Strict (Map)

import Data.ByteString.Char8 (ByteString)


-- Channel: [Nicknames]
type BotState = Map String [String]

data CommandAction = SendMessage ByteString
    | AddChat String
    | DelChat String
    | RenamePerson String String
    | ChangeFluidum String Int
    deriving Show


fst' (a,_,_) = a
snd' (_,b,_) = b
trd' (_,_,c) = c

splitOnceBy d = fmap (drop 1) . break (== d)

substringIndex what str = findIndex (what `isPrefixOf`) (tails str)