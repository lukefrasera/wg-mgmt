module Lib
  ( someFunc
  ) where


import Control.Exception
import Data.Aeson

someFunc :: IO ()
someFunc = putStrLn "someFunc"

readConfigFile :: FilePath -> IO WGConfig
readConfigFile path = do
  mbConfig <- catchJust