import System.Environment (getArgs)
import System.Console.GetOpt

import Data.Maybe (fromMaybe, fromJust)

import Listener (serverConnect)

data Options = Options {
        host     :: Maybe String,
        port     :: Maybe String,
        ipv4     :: Bool,
        botname  :: Maybe String,
        channels :: [String]
    } deriving Show


defaultOptions = Options {
        host     = Just "irc.freenode.net",
        port     = Just "6667",
        ipv4     = False,
        botname  = Just "milanbot",
        channels = []
    }

options :: [OptDescr (Options -> Options)]
options = [
        Option ['h'] ["host"]
            (OptArg ((\ h opts -> opts { host = Just h }) . fromMaybe "host")
                "HOSTNAME")
            "Host to connect to",
        Option ['p'] ["port"]
            (OptArg ((\ p opts -> opts { host = Just p }) . fromMaybe "port")
                "PORT" )
            "Port to connect to",
        Option ['4'] []
            (NoArg (\o -> o { ipv4 = True }))
                "Only use IPv4 to connect to server",
        Option ['n'] ["name"]
            (OptArg ((\ n opts -> opts { botname = Just n }) . fromMaybe "name")
                "NAME" )
            "Name of the bot",
        Option ['c'] ["channel"]
            (ReqArg (\c opts -> opts { channels = channels opts ++ [c] })
                "CHANNEL")
            "The channels to connect to. Can be specified multiple times."
    ]

compileOpts :: [String] -> IO (Options, [String])
compileOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  )  -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where
        header = "Usage: ./chatbot"

main = do
    argv <- getArgs
    (opts, args) <- compileOpts argv
    let Options {
        host     = host,
        port     = port,
        ipv4     = ipv4,
        botname  = botname,
        channels = channels
    } = opts
    serverConnect (fromJust host) (fromJust port) ipv4 channels (fromJust botname)
