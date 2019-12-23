{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances  #-}

module Lib
  ( User (..)
  , WGConfig (..)
  , Key
  , Address
  , Port
  , EndPoint
  , Name
  , PresharedKey
  , CmdString
  , KeepAlive
  , Peers
  , CIDR(..)
  , Cidr
  , readConfigFile
  , writeConfigFile
  , initConfig
  , addUserToConfig
  , addPeerAdjacent
  , getAvailableAddress
  , addIP
  , ipToOctet
  , ipToInt
  , intToOctet
  , ipFromInt
  , delUserFromConfig
  , getUserInfo
  , userInConfig
  ) where


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


data WGConfig = WGConfig [User] deriving (Generic, Show)
instance ToJSON WGConfig
instance FromJSON WGConfig

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

data User = User
  { name :: String
  , privateKey :: Key
  , publicKey :: Key
  , presharedKey :: PresharedKey
  , availableAddresses :: [CIDR]
  , port :: Port
  , endPoint :: EndPoint
  , preUp :: CmdString
  , preDown :: CmdString
  , postUp :: CmdString
  , postDown :: CmdString
  , keepAlive :: KeepAlive
  , peers :: Peers
  } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

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
      if bs == "" then return $ Right $ WGConfig [] else
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
addUserToConfig (WGConfig users) user
  | userInConfig (WGConfig users) (name user) = Left "User already in config"
  | addressInConfig (WGConfig users) address = Left $ "Address " ++ show address ++ " Taken"
  | otherwise = Right $ WGConfig (user:addPeerAdjacent users user)
  where
    address = caddr $ L.head $ availableAddresses user

addPeerAdjacent :: [User] -> User -> [User]
-- addPeerAdjacent users user | trace ("config " ++ show users ++ "\n\n user: " ++ show user ++ "\n") False = undefined
addPeerAdjacent users user =
  case peers user of
    Just npeers -> L.map (\wuser -> listAppendPeers wuser npeers) users
      where
        -- listAppendPeers u ps | trace ("User: " ++ show u ++ "\n\n Peers: " ++ show ps ++ "\n") False = undefined
        listAppendPeers u [] = u
        listAppendPeers u (p:ps) = if p == name u
          then listAppendPeers u{peers = Just unionpeers} ps
          else listAppendPeers u ps
            where
              unionpeers = S.toList . S.fromList $ name user : fromMaybe [] (peers u)
    Nothing -> users

userInConfig :: WGConfig -> Name -> Bool
userInConfig (WGConfig []) username = False
userInConfig (WGConfig users) username =
  L.any (\u -> (name u) == username) users

addressInConfig :: WGConfig -> IPv4 -> Bool
addressInConfig (WGConfig users) address =
  L.any (\u -> (caddr (L.head (availableAddresses u))) == address) users


getAvailableAddress :: WGConfig -> String
getAvailableAddress (WGConfig []) =
  error "User must supply first IP"
getAvailableAddress (WGConfig users) =
  case addIP (read (L.maximum addresses)) 1 of
    Nothing -> ""
    Just ip -> show ip
  where
    addresses = [address user | user <- users]
    address = show . caddr . L.head . availableAddresses

ipToOctet :: IPv4 -> [Int]
ipToOctet = fromIPv4


ipToInt :: IPv4 -> Integer
ipToInt =
  sum . L.map (\(n,o) -> toInteger o * 256 ^ n) . L.zip [0..] . L.reverse . ipToOctet

intToOctet :: Integer -> [Integer]
intToOctet 0 = []
intToOctet i = mod i 256:intToOctet(div i 256)

ipFromInt :: Integer -> IPv4
ipFromInt i =
  toIPv4 $ L.reverse $ L.map fromInteger (intToOctet i)

-- TODO: Check if new ipv4 is valid otherwise return Nothing
addIP :: IPv4 -> Integer-> (Maybe IPv4)
addIP ip n =
  Just (ipFromInt $ (ipToInt ip ) + n)

delUserFromConfig :: WGConfig -> Name -> Either String WGConfig
delUserFromConfig config name =
  case userInConfig config name of
    False -> Left "User not found"
    True -> Right $ removeUser config name

removeUser :: WGConfig -> Name -> WGConfig
removeUser (WGConfig users) username =
  WGConfig [user | user <- users, name user /= username]

getUserInfo :: WGConfig -> Name -> Either String String
getUserInfo config name =
  case userInConfig config name of
    False -> Left "User not found"
    True -> Right $ T.unpack $ T.decodeUtf8 $ encodePretty user
    where
      user = lookupUser config name

lookupUser :: WGConfig -> Name -> User
lookupUser (WGConfig (user:users)) username =
  if name user == username
    then user
    else lookupUser (WGConfig users) username