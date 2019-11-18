{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , readConfigFile
  , writeConfigFile
  , initConfig
  , addUserToConfig
  , getAvailableAddress
  , addIP
  , ipToOctet
  , ipToInt
  , intToOctet
  , ipFromInt
  ) where


import Control.Exception
import Data.Aeson
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
-- import Data.String.Utils
import GHC.Generics
import System.Directory
import System.IO.Error
import Debug.Trace
import Data.List
import Data.IP


data WGConfig = WGConfig [User] deriving (Generic, Show)
instance ToJSON WGConfig
instance FromJSON WGConfig

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
  , peers :: Maybe [Name]
  } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

writeConfigFile :: FilePath -> WGConfig -> IO ()
writeConfigFile configPath config = BS.writeFile configPath (Yaml.encode config)

readConfigFile :: FilePath -> IO WGConfig
readConfigFile path = do
  mbConfig <- catchJust
    (\e -> if isDoesNotExistError e then Just () else Nothing)
    (BS.readFile path >>= return . Yaml.decode)
    (\_ -> return $ Just (WGConfig []))
  case mbConfig of
    Nothing -> error "YAML file is corrupt"
    Just config -> return config

initConfig :: WGConfig -> IO (Maybe WGConfig)
initConfig config = do
  createDirectoryIfMissing True "users"
  createDirectoryIfMissing True "removed"
  return Nothing


-- TODO: determine requirements for adding a user
{- 
  - Determine if user already present
  - Check for ovrelapping address
  - 
-}
addUserToConfig :: WGConfig -> User -> Maybe WGConfig
addUserToConfig (WGConfig users) user
  | userInConfig (WGConfig users) (name user) = Nothing
  | otherwise = Just (WGConfig $ user:users)

userInConfig :: WGConfig -> Name -> Bool
userInConfig (WGConfig users) username =
  any (\u -> (name u) == username) users

addressInConfig :: WGConfig -> Address -> Bool
addressInConfig (WGConfig users) user =
  any (\u -> (address u) == (address user)) users

getAvailableAddress :: WGConfig -> String
getAvailableAddress (WGConfig users) =
  case addIP (read (maximum addresses)) 1 of
    Nothing -> ""
    Just ip -> show ip
  where
    addresses = [address user | user <- users]

ipToOctet :: IPv4 -> [Int]
ipToOctet = fromIPv4


ipToInt :: IPv4 -> Integer
ipToInt =
  sum . map (\(n,o) -> toInteger o * 256 ^ n) . zip [0..] . reverse . ipToOctet

intToOctet :: Integer -> [Integer]
intToOctet 0 = []
intToOctet i = mod i 256:intToOctet(div i 256)

ipFromInt :: Integer -> IPv4
ipFromInt i =
  toIPv4 $ reverse $ map fromInteger (intToOctet i)

-- TODO: Check if new ipv4 is valid otherwise return Nothing
addIP :: IPv4 -> Integer-> (Maybe IPv4)
addIP ip n =
  Just (ipFromInt $ (ipToInt ip ) + n)