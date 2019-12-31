{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Lib
import System.Directory
import System.Process
import System.IO
import Data.String.Utils
import qualified Data.Text as T
import qualified Control.Monad as M
import Debug.Trace
import Text.Read
import Data.IP
import qualified Data.List as L
import Data.Maybe

data Command =
    Init
  | Gen Name
  | Add CmdUser
  | Del Name
  | Get Name
  | Update CmdUser
  | Make Name
  deriving Show

data CmdUser = CmdUser
  { cname :: String
  , cprivateKey :: Maybe Key
  , cpublicKey :: Maybe Key
  , cpresharedKey :: Maybe PresharedKey
  , caddress :: Maybe [Maybe CIDR]
  , cport :: Maybe Port
  , cendpoint :: Maybe EndPoint
  , cpreUp :: Maybe CmdString
  , cpreDown :: Maybe CmdString
  , cpostUp :: Maybe CmdString
  , cpostDown :: Maybe CmdString
  , ckeepAlive :: Maybe KeepAlive
  , cpeers :: Maybe Peers
  , ctable :: Maybe Table
  } deriving Show

userParser :: Parser CmdUser
userParser = CmdUser
  <$> nameParser
  <*> optional (keyParser "private" 'p' "PRIVATE")
  <*> optional (keyParser "public" 'b' "PUBLIC")
  <*> optional (mbKeyParser "shared" 's' "SHARED")
  <*> optional mbCidrParser
  <*> optional portParser
  <*> optional endPointParser
  <*> optional (commandStringParser "pre-up" "PREUP")
  <*> optional (commandStringParser "pre-down" "PREDOWN")
  <*> optional (commandStringParser "post-up" "POSTUP")
  <*> optional (commandStringParser "post-down" "POSTDOWN")
  <*> optional keepAliveParser
  <*> optional mbPeerParser
  <*> optional tableParser

tableParser :: Parser Table
tableParser = 
  Just <$> tableparse
  <|> flag' Nothing (long "no-table")
  where
    tableparse = strOption $
      long "table"
      <> short 't'
      <> metavar "TABLE"
      <> help "Network table"

peerParser :: Parser [Name]
peerParser =
  pParser
  where
    pParser = some $ strOption $
      long "peer"
      <> short 'e'
      <> metavar "PEER"
      <> help "Peer Name."

mbPeerParser :: Parser (Maybe [Name])
mbPeerParser =
  Just <$> peerParser
  <|> flag' Nothing (long "no-peers")

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
  <|> flag' Nothing (long "no-endpoint")
  where
    endpoint = strOption $
      long "endpoint"
      <> short 'e'
      <> metavar "ENDPOINT"
      <> help "The uri:port of the users endpoint"

commandStringParser :: String -> String -> Parser CmdString
commandStringParser longstr metastr =
  Just <$> cmdstrparser
  <|> flag' Nothing (long $ "no-" ++ longstr)
  where
    cmdstrparser = some (strOption $
      long longstr
      <> metavar metastr
      <> help ("" ++ longstr ++ " commands"))

mbKeyParser :: String -> Char -> String -> Parser (Maybe Key)
mbKeyParser longStr shortChar metaStr =
  Just <$> keyParser longStr shortChar metaStr
  <|> flag' Nothing (long $ "create-" ++ longStr)
keyParser :: String -> Char -> String -> Parser Key
keyParser longStr shortChar metaStr = strOption $
  long longStr
  <> short shortChar
  <> metavar metaStr
  <> help ("Input " ++ longStr ++ "key")

portParser :: Parser Port
portParser =
  Just <$> port
  <|> flag' Nothing (long "no-port")
  where port = strOption $ long "port" <> short 't' <> metavar "PORT" <> help "Port number"

addressParser :: Parser Address
addressParser = strOption $
  long "address"
  <> short 'a'
  <> metavar "ADDRESS"
  <> help "Address of the user"

cidrParser :: Parser [String]
cidrParser = some $ strOption $
  long "available"
  <> short 'a'
  <> metavar "AVAILABLE"
  <> help "Available address of user"

mbCidrParser :: Parser [Maybe CIDR]
mbCidrParser = map cidrFromString <$> cidrParser

cidrFromString :: String -> Maybe CIDR
cidrFromString s =
  case split "/" s of
    [] -> Nothing
    x:[] -> Nothing
    saddr:smask:_ -> do
      CIDR <$> toAddress saddr <*> (makeAddrRange <$> toAddress saddr <*> toMask smask)

toAddress :: String -> Maybe IPv4
toAddress s =
  if ((length octet) == 4)
    then return $ toIPv4 . liftMbList $ octet
    else Nothing
  where
    validOctet = mapM inOctetRange octet
    octet = map readMaybe $ split "." s :: [Maybe Int]
    liftMbList [] = []
    liftMbList (x:xs) = 
      case x of
        Just value -> value:liftMbList xs
        Nothing -> liftMbList xs

inOctetRange :: Maybe Int -> Maybe Bool
inOctetRange (Just id) = Just (id >= 0)


toMask :: String -> Maybe Int
toMask = readMaybe

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
getParser = Get <$> nameParser

updateParser :: Parser Command
updateParser = Update <$> userParser

makeParser :: Parser Command
makeParser = Make
  <$> nameParser


commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ command "init"   (info (helper <*> initParser)   (progDesc "Initialize Mgmt"))
  , command "gen"    (info (helper <*> genParser)    (progDesc "Generate Conf files"))
  , command "add"    (info (helper <*> addParser)    (progDesc "Add client/server"))
  , command "del"    (info (helper <*> delParser)    (progDesc "Delete client/server"))
  , command "get"    (info (helper <*> getParser)    (progDesc "Get client/server info"))
  , command "update" (info (helper <*> updateParser) (progDesc "Update client/server configuration"))
  , command "make"   (info (helper <*> makeParser)   (progDesc "Make client/server self-installing script"))
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
  <> help "Path to mgmt configuration"

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
  Options configPath command <- execParser (info (helper <*> optionsParser) (progDesc "WireGuard MGMT"))

  homeDir <- getHomeDirectory
  let expandedDataPath = replace "~" homeDir configPath
  -- writeConfigFile expandedDataPath $ WGConfig 
  --   [ User "grapple" "privatekey" "publickey" (Just "presharedkey") "10.2.0.1" (Just "4444") (Just "lukefrasera.dynu.net") Nothing Nothing Nothing Nothing (Just 25) Nothing
  --   ]
  config <- readConfigFile expandedDataPath
  -- print config
  case config of
    Left str -> putStrLn str
    Right config -> do
      eiConfig <- run config command
      case eiConfig of
        Left str -> putStrLn $ "Error: " ++ str
        Right (WGConfig []) -> putStrLn "Nothing to write."
        Right modConfig -> writeConfigFile expandedDataPath modConfig
      return ()

generateUser :: CmdUser -> WGConfig -> IO User
generateUser cUser config = do
  -- Check if private key supplied
  let name = cname cUser
  privateKey <- do
    (_, Just hout, _, _) <- createProcess (proc "wg" ["genkey"]){std_in = CreatePipe, std_out = CreatePipe}
    strip <$> hGetContents hout
  -- syscal wg to generate key
  publicKey <- do
    (Just hin, Just hout, _, _) <- createProcess (proc "wg" ["pubkey"]){std_in = CreatePipe , std_out = CreatePipe}
    hPutStr hin privateKey
    strip <$> hGetContents hout
  -- syscall wg off of the private key
  presharedKey <-
    case cpresharedKey cUser of
      Just Nothing -> do
        (_, Just hout, _,_) <- createProcess(proc "wg" ["genpsk"]){std_in = CreatePipe, std_out = CreatePipe}
        Just . strip <$> hGetContents hout
      Just preshared -> return preshared
      Nothing -> return Nothing
  -- think about thid mechanism
  address <-
    case caddress cUser of
      Just addr -> case convertedAddrs of
          [] -> return [(CIDR (read newAddress) (read (newAddress ++ "/32")))]
          v -> return v
        where
          convertedAddrs = catMaybes addr
          newAddress = getAvailableAddress config
      Nothing -> return [(CIDR (read newAddress) (read (newAddress ++ "/32")))]
        where
          newAddress = getAvailableAddress config
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
      Nothing -> return $ Just $
        [ "ip -4 route del 10.0.0.0/9 dev %i"
        , "ip -4 route add 10.0.0.0/9 dev %i metric 601"]
      Just cmd -> return cmd
  postDown <-
    case cpostUp cUser of
      Nothing -> return Nothing
      Just cmd -> return cmd
  keepAlive <-
    case ckeepAlive cUser of
      Nothing -> return $ Just 25
      Just k -> return k
  peers <-
      case cpeers cUser of
        Nothing -> return Nothing
        Just p -> return p
  table <-
      case ctable cUser of
        Nothing -> return Nothing
        Just p -> return p
  return (User name privateKey publicKey presharedKey address port endPoint preUp preDown postUp postDown keepAlive peers table)

updateUserInConfig :: WGConfig -> CmdUser -> Either String WGConfig
updateUserInConfig config user =
  if userInConfig config $ cname user
    then Right $ updatePeers (updateUser config user) $ fromCmdUser user
    else Left "User not in config"
    where
      updatePeers (WGConfig ps) u= WGConfig $ addPeerAdjacent ps u

fromCmdUser :: CmdUser -> User
fromCmdUser (CmdUser name privatekey publickey presharedkey availableAddresses port endPoint preup predown postup postdown keepalive peers table) = User
  name
  (fromMaybe "" privatekey)
  (fromMaybe "" publickey)
  (fromMaybe Nothing presharedkey)
  ([a | Just a <- (fromMaybe [] availableAddresses)])
  (fromMaybe Nothing port)
  (fromMaybe Nothing endPoint)
  (fromMaybe Nothing preup)
  (fromMaybe Nothing predown)
  (fromMaybe Nothing postup)
  (fromMaybe Nothing postdown)
  (fromMaybe Nothing keepalive)
  (fromMaybe Nothing peers)
  (fromMaybe Nothing table)
  
updateUser :: WGConfig -> CmdUser -> WGConfig
updateuser (WGConfig []) newuser = WGConfig []
updateUser (WGConfig (user:users)) newuser =
  if name user == cname newuser
    then WGConfig $ (mergeUsers user newuser):updatedUsers
    else WGConfig $ user:updatedUsers
    where
      updatedUsers =
        case updateUser (WGConfig users) newuser of
          (WGConfig users) -> users
updateUser (WGConfig users) newuser =
  case users of
    [] -> WGConfig []
    users -> WGConfig users

mergeUsers :: User -> CmdUser -> User
mergeUsers ouser nuser = User
  (name ouser)
  (updateField (cprivateKey nuser) (privateKey ouser))
  (updateField (cpublicKey nuser) (publicKey ouser))
  (updateField (cpresharedKey nuser) (presharedKey ouser))
  (updateField (catMaybes <$> caddress nuser) (availableAddresses ouser))
  (updateField (cport nuser) (port ouser))
  (updateField (cendpoint nuser) (endPoint ouser))
  (updatePeers (cpreUp nuser) (preUp ouser))
  (updatePeers (cpreDown nuser) (preDown ouser))
  (updatePeers (cpostUp nuser) (postUp ouser))
  (updatePeers (cpostDown nuser) (postDown ouser))
  (updateField (ckeepAlive nuser) (keepAlive ouser))
  (updatePeers (cpeers nuser) (peers ouser))
  (updateField (ctable nuser) (table ouser))
  where
    toCIDR ((a, r):xs) = (read a, read r):toCIDR xs
    toCIDR [] = []
    -- updateField a b | trace ("updareField " ++ show a ++ " " ++ show b) False = undefined
    updateField (Just value) _ = value
    updateField Nothing value = value
    -- updatePeers a b | trace ("updatePeers " ++ show a ++ " " ++ show b) False = undefined
    updatePeers (Just (Just [])) value = value
    updatePeers (Just (Just value)) _ = Just value
    updatePeers (Just Nothing) _ = Nothing
    updatePeers Nothing value = value

getUser :: WGConfig -> Name -> User
getUser (WGConfig (user:users)) username = 
  if name user == username then user
  else getUser (WGConfig users) username

genConfStr :: WGConfig -> Name -> Either String String
genConfStr config username =
  if userInConfig config username
    then Right confstr
    else Left "User not in file."
    where
      confstr = createStr $ getUser config username
      showCIDR (CIDR{caddr, crange}) = (show caddr ++ "/" ++ (show . snd . addrRangePair) crange)
      createStr (User name prvkey pubkey pshkey addr port ep preup predown postup postdown keepalive peers table) =
        join "\n" [ x | Just x <-
          [ Just "[Interface]"
          , Just $ "Address = " ++ (join "," $ map showCIDR addr)
          , Just $ "PrivateKey = " ++ prvkey
          , "ListenPort = " `combine` port
          , "Table = " `combine` table
          , cmdSection "PreUp = " preup
          , cmdSection "PostUp = " postup
          , cmdSection "PreDown = " predown
          , cmdSection "PostDown = " postdown
          , Just ""
          , peersSection peers
          ]]
      combine _ Nothing = Nothing
      combine str (Just value) = Just $ str ++ value
      peersSection Nothing = Nothing
      peersSection (Just peerList) =
        Just $ join "\n\n" $ map ((createPeerSection $ getUser config username) . getUser config ) peerList

cmdSection :: String -> Maybe [String] -> Maybe String
cmdSection _ Nothing = Nothing
cmdSection str (Just cmds) = Just $ join "\n" $ map (str ++) cmds

createPeerSection :: User -> User -> String
-- createPeerSection User{publicKey, presharedKey, address, endPoint, keepAlive} | trace ("createPeerSection " ++ show publicKey ++ " " ++ show presharedKey ++ " " ++ show address ++ " " ++ show endPoint ++ " " ++ show keepAlive) False = undefined
createPeerSection User{presharedKey} User{name, publicKey, availableAddresses, endPoint, keepAlive} =
  join "\n" [x | Just x <-
    [ Just $ "#=== [" ++ name ++ "] ===#"
    , Just "[Peer]"
    , "PublicKey = " `combine` Just publicKey
    , "PresharedKey = " `combine` presharedKey
    , "AllowedIPs = " `combine` (Just $ join "," $ map showCIDR availableAddresses)
    , "Endpoint = " `combine` endPoint
    , "PersistentKeepalive = " `combine` (show <$> keepAlive)
    ]]
  where
    combine _ Nothing = Nothing
    combine str (Just value) = Just $ str ++ value
    showCIDR (CIDR{caddr, crange}) = (show caddr ++ "/" ++ (show . snd . addrRangePair) crange)

run :: WGConfig -> Command -> IO (Either String WGConfig)
run config Init          = initConfig config
run config (Gen name)    =
  case genConfStr config name of
    Left str -> return $ Left str
    Right str -> do
      putStrLn str
      return $ Right config
run config (Add cuser)   =
  if userInConfig config $ cname cuser
  then return $ Left "User already in config"
  else do
    user <- generateUser cuser config
    return (addUserToConfig config user)
run config (Del name)    = return $ delUserFromConfig config name
run config (Get name)    =
  case getUserInfo config name of
    Left str -> return $ Left str
    Right str -> do
      putStrLn str
      return $ Right config
run config (Update cuser) = do
  print cuser
  return $ updateUserInConfig config cuser
-- run config (Make name)   = putStrLn "Make a self-installing script"
