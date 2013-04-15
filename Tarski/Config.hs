module Tarski.Config (
         Config,
         filePath,
         testCases,
         methodName,
         readConfig
       ) where

import Data.Text ( pack )

import Data.Configurator (load, Worth(..), lookupDefault, require)


data Config = Config {
                      filePath :: String,
                      testCases :: String,
                      methodName :: String
                     }



readConfig :: String -> IO Config
readConfig f = do config <- load [Required f]
                  fp <- require config $ pack "file-path"
                  tc <- require config $ pack "paren-test"
                  mn <- require config $ pack "method-name"
                  return Config { filePath=fp,
                                  testCases=tc,
                                  methodName=mn
                                }
