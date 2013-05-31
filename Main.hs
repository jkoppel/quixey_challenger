import System.Process
import System.Environment ( getArgs )
import Debug.Trace
import Data.Text (pack, unpack, strip)
import Data.Text.Read (hexadecimal)
import qualified Data.Map as M

import Data.List

import Control.Lens.Getter ( (^.) )

import Language.Java.Syntax
import Language.Java.Pretty

import Tarski.Config ( Config, readConfig, filePath, testCases, methodName, holeDepth, maxUnrollDepth )
import Tarski.Mutate ( constantFold )
import Tarski.Symbolic
import Tarski.Sketch

--type Tests = [([Int],Int)] -- modify this? more general. can't be configurable as types are done

main :: IO ()
main = do args <- getArgs
          cfg <- case args of
                      [s] -> readConfig s
                      _   -> error "QC requires a single argument denoting a configuration file"
          mainLoop cfg


mainLoop :: Config -> IO ()
mainLoop cfg = do
    let file = cfg ^. filePath
        methodname = cfg ^. methodName
        holedepth = cfg ^. holeDepth
    program <- readFile file
    let (state, ideas, qs) = genSketches program methodname holedepth
    best <- test_ideas state (reverse ideas) (reverse qs) cfg
    case best of
        Nothing -> putStrLn $ unlines $ map prettyPrint ideas
        Just (code, model) -> putStrLn $ prettyPrint $ constantFold model code

test_ideas :: SketchState -> [MemberDecl] -> [MemberDecl] -> Config -> IO (Maybe (MemberDecl, M.Map String Int))
test_ideas st [] _ _ = return $ Nothing
test_ideas st (idea:ideas) (q:qs) cfg = do
    result <- test_idea st idea q cfg
    case result of
        Nothing -> test_ideas st ideas qs cfg
        Just model -> return $ Just (idea, model)

test_idea :: SketchState -> MemberDecl -> MemberDecl -> Config -> IO (Maybe (M.Map String Int))
test_idea st idea q cfg = do
    let tests     = cfg ^. testCases
        maxunroll = cfg ^. maxUnrollDepth
        z3in = evalSketch idea st tests maxunroll
    putStrLn $ prettyPrint q
    writeFile "z3.smt2" z3in
    appendFile "z3.smt2" "(get-model)"
    (exit, out, err) <- readProcessWithExitCode "z3" ["z3.smt2"] ""
    let (head:model) = lines out
    writeFile "output.smt2" out
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
