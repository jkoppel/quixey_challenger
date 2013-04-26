import System.Process
import System.IO
import System.Random
import System.Exit
import System.Environment ( getArgs )
import Control.Monad
import Debug.Trace
import Data.Int
import Data.List.Split
import System.Timeout
import System.Directory
import Mutate
import Data.Text (pack, unpack, strip)
import Data.Text.Read (hexadecimal)
import qualified Data.Map as M

import Data.List

import Language.Java.Syntax
import Language.Java.Pretty
import Symbolic hiding (maxUnrollDepth)
import Sketch

import Tarski.Config ( Config, readConfig, filePath, testCases, methodName, holeDepth, maxUnrollDepth )

type Tests = [([Int],Int)] -- modify this to be configurable?

-- deals with configuration file
main :: IO ()
main = do args <- getArgs
          cfg <- case args of
                      [s] -> readConfig s
                      _   -> error "QC requires a single argument denoting a configuration file"
          mainLoop cfg


mainLoop :: Config -> IO ()
mainLoop cfg = do
    let file = filePath cfg
        tests = testCases cfg
        methodname = methodName cfg
        holedepth = holeDepth cfg
        maxunroll = maxUnrollDepth cfg
    program <- readFile file
    (state, ideas, qs) <- return $ genSketches program methodname holedepth
    best <- test_ideas state (reverse ideas) (reverse qs) tests maxunroll
    case best of
        Nothing -> putStrLn $ unlines $ map prettyPrint ideas
        Just (code, model) -> do
            final_code <- return $ (constantFold model code) :: IO MemberDecl
            putStrLn ((prettyPrint final_code) :: String)--((show model) ++ " " ++ (prettyPrint code))

test_ideas :: SketchState -> [MemberDecl] -> [MemberDecl] -> Tests -> Int -> IO (Maybe (MemberDecl, M.Map String Int))
test_ideas st [] _ _ _ = return $ Nothing
test_ideas st (idea:ideas) (q:qs) tests maxunroll = do
    result <- test_idea st idea q tests maxunroll
    case result of
        Nothing -> test_ideas st ideas qs tests maxunroll
        Just model -> return $ Just (idea, model)

test_idea :: SketchState -> MemberDecl -> MemberDecl -> Tests -> Int -> IO (Maybe (M.Map String Int))
test_idea st idea q tests maxunroll = do
    putStrLn $ prettyPrint q
    z3in <- return $ ({-"(set-logic QF_AUFBV)\n" ++ -}(evalSketch idea st tests maxunroll))
    writeFile "z3.smt2" z3in
    (exit, out, err) <- readProcessWithExitCode "z3" ["z3.smt2"] ""
    (head:model) <- return $ lines out
    if head == "unsat"
     then return Nothing
     else return $ Just (str_to_map $ tail model)

str_to_map :: [String] -> M.Map String Int
str_to_map [] = M.empty
str_to_map [x] = M.empty
str_to_map (x:y:rest) =
    if (length xs > 1 && isPrefixOf "sketch" (xs !! 1))
    then
    let
        val = if ylen == 1 then yval else -yval
        ylen = length $ words y
        yval = if ylen == 1 then to_int y else to_int $ (words y) !! 1
        m = str_to_map rest
    in M.insert (xs !! 1) val m
    else str_to_map (y:rest)
    where
    xs = words x


to_int :: String -> Int
to_int s = ans
    where
    trimmed = unpack $ strip $ pack $ s
    drop_sharp = drop 2 trimmed
    drop_paren = init $ drop_sharp
    Right (ans, _) = hexadecimal $ pack $ drop_paren
