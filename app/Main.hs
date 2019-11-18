{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Lib
import System.Directory
import System.Process
import System.IO
import Data.String.Utils
import qualified Data.Text as T

data Command =
    Init
  | Gen Name
  | Add CmdUser
  | Del Name
  | Get
  | Update CmdUser
  | Make Name
  deriving Show

data CmdUser = CmdUser
  { cname :: String
  , cprivateKey :: Maybe Key
  , cpublicKey :: Maybe Key
  , cpresharedKey :: Maybe PresharedKey
  , caddress :: Maybe Address
  , cport :: Maybe Port
  , cendpoint :: Maybe EndPoint
  , cpreUp :: Maybe CmdString
  , cpreDown :: Maybe CmdString
  , cpostUp :: Maybe CmdString
  , cpostDown :: Maybe CmdString
  , ckeepAlive :: Maybe KeepAlive
  , cpeers :: Maybe [Name]
  } deriving Show

userParser :: Parser CmdUser
userParser = CmdUser
  <$> nameParser
  <*> optional (keyParser "private" 'p' "PRIVATE")
  <*> optional (keyParser "public" 'b' "PUBLIC")
  <*> optional (mbKeyParser "shared" 's' "SHARED")
  <*> optional addressParser
  <*> optional portParser
  <*> optional endPointParser
  <*> optional (commandStringParser "pre-up" "PREUP")
  <*> optional (commandStringParser "pre-down" "PREDOWN")
  <*> optional (commandStringParser "post-up" "POSTUP")
  <*> optional (commandStringParser "post-down" "POSTDOWN")
  <*> optional keepAliveParser
  <*> optional peerParser

peerParser :: Parser [Name]
peerParser =
  pParser
  where
    pParser = many $ strOption $
      long "peer"
      <> short 'e'
      <> metavar "PEER"
      <> help "Peer Name."

keepAliveParser :: Parser KeepAlive
keepAliveParser =
  Just <$> keepalive
  where
    keepalive = option auto $
      long "keep-alive"
      <> metavar "KEEPALIVE"
      <> help "number of seconds between messages"

endPointParser :: Parser EndPoint
endPointParser =
  Just <$> endpoint
  where
    endpoint = strOption $
      long "endpoint"
      <> short 'e'
      <> metavar "ENDPOINT"
      <> help "The uri:port of the users endpoint"

commandStringParser :: String -> String -> Parser CmdString
commandStringParser longstr metastr =
  Just <$> cmdstrparser
  where
    cmdstrparser = many (strOption $
      long longstr
      <> metavar metastr
      <> help ("" ++ longstr ++ " commands"))

mbKeyParser :: String -> Char -> String -> Parser (Maybe Key)
mbKeyParser longStr shortChar metaStr =
  Just <$> (keyParser longStr shortChar metaStr)
keyParser :: String -> Char -> String -> Parser Key
keyParser longStr shortChar metaStr = strOption $
  long longStr
  <> short shortChar
  <> metavar metaStr
  <> help ("Input " ++ longStr ++ "key")

portParser :: Parser Port
portParser =
  Just <$> port
  where port = strOption $ long "port" <> short 't' <> metavar "PORT" <> help "Port number"

addressParser :: Parser Address
addressParser = strOption $
  long "address"
  <> short 'a'
  <> metavar "ADDRESS"
  <> help "Address of the user"

initParser :: Parser Command
initParser = pure Init

genParser :: Parser Command
genParser = Gen <$> nameParser

nameParser :: Parser Name
nameParser = argument str $
  metavar "NAME"
  <> help "Name of the User"

addParser :: Parser Command
addParser = Add
  <$> userParser

delParser :: Parser Command
delParser = Del <$> nameParser

getParser :: Parser Command
getParser = pure Get

updateParser :: Parser Command
updateParser = Update <$> userParser

makeParser :: Parser Command
makeParser = Make
  <$> nameParser


commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ command "init"   (info initParser   (progDesc "Initialize Mgmt"))
  , command "gen"    (info genParser    (progDesc "Generate Conf files"))
  , command "add"    (info addParser    (progDesc "Add client/server"))
  , command "del"    (info delParser    (progDesc "Delete client/server"))
  , command "get"    (info getParser    (progDesc "Get client/server info"))
  , command "update" (info updateParser (progDesc "Update client/server configuration"))
  , command "make"   (info makeParser   (progDesc "Make client/server self-installing script"))
  ]

type ItemIndex = Int
type ItemDescription = Maybe String
data Options = Options FilePath Command deriving Show

defaultDataPath :: FilePath
defaultDataPath = ".wg-mgmt.yaml"

optionsParser :: Parser Options
optionsParser = Options
  <$> dataPathParser
  <*> commandParser

dataPathParser :: Parser FilePath
dataPathParser = strOption $
  value defaultDataPath
  <> long "config"
  <> short 'c'
  <> metavar "CONFIGURATION"
  <> help ("Path to mgmt configuration")

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser =
  strOption (long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description")

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
  Just <$> itemDescriptionValueParser
  <|> flag' Nothing (long "clear-desc") -- "--clear-desc"

main :: IO ()
main = do
  Options configPath command <- execParser (info (optionsParser) (progDesc "WireGuard MGMT"))

  homeDir <- getHomeDirectory
  let expandedDataPath = replace "~" homeDir configPath
  -- writeConfigFile expandedDataPath $ WGConfig 
  --   [ User "grapple" "privatekey" "publickey" (Just "presharedkey") "10.2.0.1" (Just "4444") (Just "lukefrasera.dynu.net") Nothing Nothing Nothing Nothing (Just 25) Nothing
  --   ]
  config <- readConfigFile expandedDataPath
  print config
  mbConfig <- run config command
  case mbConfig of
    Nothing -> writeConfigFile expandedDataPath config
    Just modConfig -> writeConfigFile expandedDataPath modConfig
  return ()

generateUser :: CmdUser -> WGConfig -> IO User
generateUser cUser config = do
  -- Check if private key supplied
  let name = cname cUser
  privateKey <- do
    (_, Just hout, _, _) <- createProcess (proc "wg" ["genkey"]){std_in = CreatePipe, std_out = CreatePipe}
    hGetContents hout
  -- syscal wg to generate key
  publicKey <- do
    (Just hin, Just hout, _, _) <- createProcess (proc "wg" ["pubkey"]){std_in = CreatePipe , std_out = CreatePipe}
    hPutStr hin privateKey
    hGetContents hout
  -- syscall wg off of the private key
  presharedKey <-
    case cpresharedKey cUser of
      Just preshared -> return preshared
      Nothing -> return Nothing
  -- think about thid mechanism
  address <-
    case caddress cUser of
      Just addr -> return addr
      Nothing -> return $ getAvailableAddress config 
  -- generate available ip
  port <-
    case cport cUser of
      Nothing -> return Nothing
      Just port -> return port
  -- use only if specififed
  endPoint <-
    case cendpoint cUser of
      Nothing -> return Nothing
      Just uri -> return uri
  -- user only if specified
  preUp <-
    case cpreUp cUser of
      Nothing -> return Nothing
      Just cmd -> return cmd
  preDown <-
    case cpreDown cUser of
      Nothing -> return Nothing
      Just cmd -> return cmd
  postUp <-
    case cpostUp cUser of
      Nothing -> return Nothing
      Just cmd -> return cmd
  postDown <-
    case cpostUp cUser of
      Nothing -> return Nothing
      Just cmd -> return cmd
  keepAlive <-
    case ckeepAlive cUser of
      Nothing -> return $ Just 25
      Just k -> return k
  let peers = cpeers cUser
  return (User name privateKey publicKey presharedKey address port endPoint preUp preDown postUp postDown keepAlive peers)


run :: WGConfig -> Command -> IO (Maybe WGConfig)
run config Init          = initConfig config
-- run config (Gen name)    = putStrLn $ "Generate Client/Server Configuration file for " ++ name
run config (Add cuser)    = do
  user <- generateUser cuser config
  return (addUserToConfig config user)
-- run config (Del name)    = putStrLn "Delete Client/Server"
-- run config Get           = putStrLn "Get Client/Server Info"
-- run config (Update user) = putStrLn "Update Client/Server Configuration Live"
-- run config (Make name)   = putStrLn "Make a self-installing script"
