{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib
  -- ( User (..)
  -- , UserDefaults (..)
  -- , WGConfig (..)
  -- , Key
  -- , Address
  -- , Port
  -- , EndPoint
  -- , Name
  -- , PresharedKey
  -- , CmdString
  -- , KeepAlive
  -- , Peers
  -- , Table
  -- , CIDR(..)
  -- , Cidr
  -- , readConfigFile
  -- , writeConfigFile
  -- , initConfig
  -- , addUserToConfig
  -- , addPeerAdjacent
  -- , getAvailableAddress
  -- , addIP
  -- , ipToOctet
  -- , ipToInt
  -- , intToOctet
  -- , ipFromInt
  -- , delUserFromConfig
  -- , getUserInfo
  -- , userInConfig
  -- ) where
where


import Control.Exception
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
-- import Data.String.Utils
import GHC.Generics
import System.Directory
import System.IO.Error
import Debug.Trace
import qualified Data.List as L
import Data.IP
import qualified Data.Text.Lazy.Encoding as T
import Data.Text.Lazy as T
import qualified Data.Text as DT
import Data.Maybe
import qualified Data.Set as S
import Control.Lens




type Key = String
type Address = String
type Subnet = String
type Cidr = (Address, Subnet)
-- type CIDR = (IPv4, AddrRange IPv4)
type Port = Maybe String
type EndPoint = Maybe String
type Name = String
type PresharedKey = Maybe Key
type CmdString = Maybe [String]
type KeepAlive = Maybe Int
type Peers = Maybe [Name]
type Table = Maybe String

data CIDR = CIDR
  { caddr :: IPv4
  , crange :: AddrRange IPv4
  } deriving (Show, Generic)
instance ToJSON CIDR
instance FromJSON CIDR
  -- parseJSON = withObject "CIDR" $ \v -> CIDR
  --   <$> v .: "addr"
  --   <*> v .: "range"

instance ToJSON IPv4 where
  toJSON ip = String . DT.pack . show $ ip
  -- toEncoding ip = Encoding' . String . DT.pack . show $ ip
instance Show a => ToJSON (AddrRange a) where
  toJSON range = String . DT.pack . show $ range

instance FromJSON IPv4 where
  parseJSON = withText "ipv4" (\x -> return (read . DT.unpack $ x))

instance FromJSON (AddrRange IPv4) where
  parseJSON = withText "range" (\x -> return (read . DT.unpack $ x))

data UserDefaults = UserDefaults
  { _dPresharedKey :: PresharedKey
  , _dPostUp :: CmdString
  , _dPostDown :: CmdString
  , _dPreUp :: CmdString
  , _dPreDown :: CmdString
  , _dPeers :: Peers
  , _dTable ::Table
  , _dKeepAlive :: KeepAlive
  } deriving (Show, Generic)
instance ToJSON UserDefaults
instance FromJSON UserDefaults
makeLenses ''UserDefaults

data User = User
  { _name :: String
  , _privateKey :: Key
  , _publicKey :: Key
  , _presharedKey :: PresharedKey
  , _availableAddresses :: [CIDR]
  , _port :: Port
  , _endPoint :: EndPoint
  , _preUp :: CmdString
  , _preDown :: CmdString
  , _postUp :: CmdString
  , _postDown :: CmdString
  , _keepAlive :: KeepAlive
  , _peers :: Peers
  , _table :: Table
  } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User
makeLenses ''User

data WGConfig = WGConfig { _defaults :: UserDefaults,  _users :: [User] } deriving (Generic, Show)
instance ToJSON WGConfig
instance FromJSON WGConfig
makeLenses ''WGConfig

initialDefaults :: UserDefaults
initialDefaults = UserDefaults {_dPresharedKey=Nothing, _dPostUp=Nothing, _dPostDown=Nothing, _dPreUp=Nothing, _dPreDown=Nothing, _dPeers=Nothing, _dTable=Nothing, _dKeepAlive=Nothing}

writeConfigFile :: FilePath -> WGConfig -> IO ()
writeConfigFile configPath config = BS.writeFile configPath (Yaml.encode config)

readConfigFile :: FilePath -> IO (Either String WGConfig)
readConfigFile path = do
  mbBs <- catchJust
    (\e -> if isDoesNotExistError e then Just () else Nothing)
    (Just <$> BS.readFile path)
    (\_ -> return $ Just BS.empty)
  case mbBs of
    Nothing -> return $ Left "YAML file is corrupt"
    Just bs ->
      if bs == "" then return $ Right $ WGConfig initialDefaults [] else
      case Yaml.decodeEither' bs of
        Left e -> return $ Left $ Yaml.prettyPrintParseException e
        Right config -> return $ Right config

initConfig :: WGConfig -> IO (Either String WGConfig)
initConfig config = do
  createDirectoryIfMissing True "users"
  createDirectoryIfMissing True "removed"
  return $ Right config


-- TODO: determine requirements for adding a user
{- 
  - Determine if user already present
  - Check for ovrelapping address
  - 
-}
addUserToConfig :: WGConfig -> User -> Either String WGConfig
addUserToConfig (WGConfig defaults users) user
  | userInConfig (WGConfig defaults users) (_name user) = Left "User already in config"
  | addressInConfig (WGConfig defaults users) address = Left $ "Address " ++ show address ++ " Taken"
  | otherwise = Right $ WGConfig defaults (user:addPeerAdjacent users user)
  where
    address = caddr $ L.head $ _availableAddresses user

addPeerAdjacent :: [User] -> User -> [User]
-- addPeerAdjacent users user | trace ("config " ++ show users ++ "\n\n user: " ++ show user ++ "\n") False = undefined
addPeerAdjacent users user =
  case _peers user of
    Just npeers -> L.map (`listAppendPeers` npeers) users
      where
        -- listAppendPeers u ps | trace ("User: " ++ show u ++ "\n\n Peers: " ++ show ps ++ "\n") False = undefined
        listAppendPeers u [] = u
        listAppendPeers u (p:ps) = if p == _name u
          then listAppendPeers u{_peers = Just unionpeers} ps
          else listAppendPeers u ps
            where
              unionpeers = S.toList . S.fromList $ _name user : fromMaybe [] (_peers u)
    Nothing -> users

userInConfig :: WGConfig -> Name -> Bool
userInConfig (WGConfig defaults []) _ = False
userInConfig (WGConfig defaults users) username =
  L.any (\u -> _name u == username) users

addressInConfig :: WGConfig -> IPv4 -> Bool
addressInConfig (WGConfig defaults users) address =
  L.any (\u -> caddr (L.head (_availableAddresses u)) == address) users


getAvailableAddress :: WGConfig -> String
getAvailableAddress (WGConfig defaults []) =
  error "User must supply first IP"
getAvailableAddress (WGConfig defaults users) =
  maybe "" show (addIP (L.maximum addresses) 1)
  where
    addresses = [address user | user <- users]
    address = caddr . L.head . _availableAddresses

ipToOctet :: IPv4 -> [Int]
ipToOctet = fromIPv4


ipToInt :: IPv4 -> Integer
ipToInt =
  sum . L.zipWith (\n o -> toInteger o * 256 ^ n) [0 .. ] . L.reverse . ipToOctet

intToOctet :: Integer -> [Integer]
intToOctet 0 = []
intToOctet i = mod i 256:intToOctet(div i 256)

ipFromInt :: Integer -> IPv4
ipFromInt i =
  toIPv4 $ L.reverse $ L.map fromInteger (intToOctet i)

-- TODO: Check if new ipv4 is valid otherwise return Nothing
addIP :: IPv4 -> Integer-> (Maybe IPv4)
addIP ip n =
  Just (ipFromInt $ ipToInt ip + n)

delUserFromConfig :: WGConfig -> Name -> Either String WGConfig
delUserFromConfig config _name =
  if userInConfig config _name then
    Right $ removeUser config _name
  else
    Left "User not found"

removeUser :: WGConfig -> Name -> WGConfig
removeUser (WGConfig defaults users) username =
  WGConfig defaults [user | user <- users, _name user /= username]

getUserInfo :: WGConfig -> Name -> Either String String
getUserInfo config name =
  if userInConfig config name then
    Right $ T.unpack $ T.decodeUtf8 $ encodePretty user
  else
    Left "User not found"
    where
      user = lookupUser config name

lookupUser :: WGConfig -> Name -> User
lookupUser (WGConfig defaults (user:users)) username =
  if _name user == username
    then user
    else lookupUser (WGConfig defaults users) username
