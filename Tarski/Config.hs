module Tarski.Config (
         Config,
         filePath,
         testCases,
         methodName,
         readConfig,
         holeDepth,
         maxUnrollDepth
       ) where

import Data.Text ( pack )

import Data.Configurator (load, Worth(..), lookupDefault, require)

type Tests = [([Int],Int)]

data Config = Config {
                      filePath :: String,
                      testCases :: Tests,
                      methodName :: String,
                      holeDepth :: Int,
                      maxUnrollDepth :: Int
                     }



readConfig :: String -> IO Config
readConfig f = do config <- load [Required f]
                  fp  <- require config $ pack "file-path"
                  tc  <- require config $ pack "paren-test"
                  mn  <- require config $ pack "method-name"
                  hd  <- require config $ pack "hole-depth"
                  mud <- require config $ pack "max-unroll-depth"
                  return Config { filePath=fp,
                                  testCases=(read tc :: Tests),
                                  methodName=mn,
                                  holeDepth=hd,
                                  maxUnrollDepth=mud
                                }
