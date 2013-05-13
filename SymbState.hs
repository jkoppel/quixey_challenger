module SymbState where

import Control.Monad.State
import Control.Lens ( makeLenses )

import Data.Map as Map

import qualified SMTLib2 as Smt
import qualified SMTLib2.Core as Smt
import qualified SMTLib2.Int as Smt
import qualified SMTLib2.BitVector as Smt
import qualified SMTLib2.Array as Smt

import Sketch

data SymbState = SymbState { _counter :: Int,
                             _smt :: [Smt.Command],
                             _pathGuard :: [Smt.Expr],
                             _retVar :: Smt.Name,
                             _varLab :: Map.Map String Int, -- SSA; Single Static Assignment
                             _sketchState :: SketchState,
                             _unrollDepth :: Int,
                             _maxUnrollDepth :: Int
                             }

makeLenses ''SymbState

startState :: SketchState -> Int -> SymbState
startState skst maxunroll = SymbState {_counter = 0,
                                       _smt = [], -- this should start with logic and options
                                       _pathGuard = [],
                                       _retVar = Smt.N "",
                                       _varLab = Map.insert "A" 0 Map.empty, -- prob not needed
                                       _sketchState = skst,
                                       _unrollDepth = 0,
                                       _maxUnrollDepth = maxunroll}

type Symb = State SymbState
