module Main where

import Options.Applicative
import Lib

data Command =
    Init 
  | Gen Name
  | Add CmdUser
  | Del Name
  | Get
  | Update CmdUser
  | Make Name
  deriving Show

type Key = String
type Address = String
type Port = Maybe String
type EndPoint = Maybe String
type Name = String
type PresharedKey = Maybe Key
type CmdString = Maybe [String]
type KeepAlive = Maybe Int

data User = User
  { name :: String
  , privateKey :: Key
  , publicKey :: Key
  , presharedKey :: PresharedKey
  , address :: Address
  , port :: Port
  , endPoint :: EndPoint
  , preUp :: CmdString
  , preDown :: CmdString
  , postUp :: CmdString
  , postDown :: CmdString
  , keepAlive :: KeepAlive
  } deriving Show

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
  Options config command <- execParser (info (optionsParser) (progDesc "WireGuard MGMT"))
  run config command

run :: FilePath -> Command -> IO()
run config Init          = putStrLn "Initializing Workspace."
run config (Gen name)    = putStrLn $ "Generate Client/Server Configuration file for " ++ name
run config (Add user)    = putStrLn $ "Add Client/Server" ++ show user
run config (Del name)    = putStrLn "Delete Client/Server"
run config Get           = putStrLn "Get Client/Server Info"
run config (Update user) = putStrLn "Update Client/Server Configuration Live"
run config (Make name)   = putStrLn "Make a self-installing script"
