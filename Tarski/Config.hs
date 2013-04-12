module Tarski.Config (
         Config,
         filePath,
         testCases,
         readConfig
       ) where

import Data.Text ( pack )

import Data.Configurator (load, Worth(..), lookupDefault, require)


data Config = Config {
                      filePath :: String,
                      testCases :: String
                     }



readConfig :: String -> IO Config
readConfig f = do config <- load [Required f]
                  fp <- require config $ pack "file-path"
                  tc <- require config $ pack "paren-test"
                  return Config { filePath=fp,
                                  testCases=tc
                                }
