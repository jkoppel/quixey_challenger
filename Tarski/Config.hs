{-# LANGUAGE TemplateHaskell #-}

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

import Control.Lens ( makeLenses )

import Data.Configurator (load, Worth(..), lookupDefault, require)

type Tests = [([Int],Int)]

data Config = Config {
                      _filePath :: String,
                      _testCases :: Tests,
                      _methodName :: String,
                      _holeDepth :: Int,
                      _maxUnrollDepth :: Int
                     }

makeLenses ''Config

readConfig :: String -> IO Config
readConfig f = do config <- load [Required f]
                  fp  <- require config $ pack "file-path"
                  tc  <- require config $ pack "paren-test"
                  mn  <- require config $ pack "method-name"
                  hd  <- require config $ pack "hole-depth"
                  mud <- require config $ pack "max-unroll-depth"
                  return Config {
                                  _filePath=fp,
                                  _testCases=(read tc :: Tests),
                                  _methodName=mn,
                                  _holeDepth=hd,
                                  _maxUnrollDepth=mud
                                }
