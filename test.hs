import System.Process
import System.IO
import System.Random
import System.Exit
import Control.Monad
import Debug.Trace
import Data.Int
import Data.List.Split
import System.Timeout
import System.Directory
import Mutate
import Data.Text (pack, unpack,  strip)
import Data.Text.Read (hexadecimal)
import qualified Data.Map as M

import Language.Java.Syntax
import Language.Java.Pretty
import Symbolic
import Sketch

main = mainLoop "TRI.java"

mainLoop :: String -> IO ()
mainLoop file = do
    program <- readFile file
    (state, ideas) <- return $ genSketches program "main"
    best <- test_ideas state ideas
    case best of
        Nothing -> putStrLn "failed"
        Just (code, model) -> do
            final_code <- return $ (constantFold model code) :: IO MemberDecl
            putStrLn ((prettyPrint final_code) :: String)--((show model) ++ " " ++ (prettyPrint code))

test_ideas :: SketchState -> [MemberDecl] -> IO (Maybe (MemberDecl, M.Map String Int))
test_ideas st [] = return $ Nothing
test_ideas st (idea:ideas) = do
    result <- test_idea st idea
    case result of
        Nothing -> test_ideas st ideas
        Just model -> return $ Just (idea, model) 
        
test_idea :: SketchState -> MemberDecl -> IO (Maybe (M.Map String Int))
test_idea st idea = do
    tests <- mapM (\_ -> make_in) [1..3]
    z3in <- return $ evalSketch idea st tests
    writeFile "z3.smt2" z3in
    (exit, out, err) <- readProcessWithExitCode "z3" ["z3.smt2"] ""
    (head:model) <- return $ lines out
    if head == "unsat"
    then return Nothing
    else return $ Just (str_to_map $ tail model)

make_in :: IO ([Int], Int)
make_in = do
    x <- randomRIO (1 :: Int, 1000) 
    return $ ([x], x*(x+1) `div` 2)
        
str_to_map :: [String] -> M.Map String Int
str_to_map [] = M.empty
str_to_map [x] = M.empty
str_to_map (x:y:rest) = M.insert var val m
    where
    var = (words x) !! 1
    val = if ylen == 1 then yval else -yval
    ylen = length $ words y
    yval = if ylen == 1 then to_int y else to_int $ (words y) !! 1
    m = str_to_map rest

to_int :: String -> Int
to_int s = ans
    where
    trimmed = unpack $ strip $ pack $ s
    drop_sharp = drop 2 trimmed
    drop_paren = init $ drop_sharp
    Right (ans, _) = hexadecimal $ pack $ drop_paren
        
test_cases = 10000
max_array_size = 6
max_int = 1000*1000

-- MAIN CODE
challenges = [("GCD", gcd_in, gcd_check),
              ("MAX_SUBSET", mw_in, mw_check),
              ("MOD_INVERSE", mod_inv_in, mod_inv_check),
              ("BINARY_SEARCH", binary_search_in, binary_search_check),
              ("NESTED_PARENS", nested_parens_in, nested_parens_check),
              ("ADD", add_in, add_check),
              ("NEXT_PAL", next_pal_in, next_pal_check)
             ]
{-main = do
    fix 6
    --good <- run_cases test_cases "MOD_INVERSE" mod_inv_in mod_inv_check
    --putStrLn $ show good
-}

fix :: Int -> IO ()
fix n = do
    program <- readFile orig_file
    rng <- getStdGen
    loop n program rng
    where
    (name, _, _) = challenges !! n
    file = name ++ ".java"
    orig_file = name ++ "_orig.java"

loop :: Int -> String -> StdGen -> IO ()
loop n program rng = do
    removeFile file
    writeFile file mutated
    putStrLn "COMPILING"
    p <- runCommand ("javac " ++ file)
    javac <- trace mutated (waitForProcess p)
    case javac of
        ExitFailure _ -> loop n program rng'
        ExitSuccess -> do
            putStrLn "JAVAC PASSED"
            good <- run_cases test_cases name input output
            putStrLn "TEST CASES RUN"
            putStrLn $ show good
            if good
            then return ()
            else loop n program rng'
    where
    (name, input, output) = challenges !! n
    file = name ++ ".java"
    (mutated, rng') = mutateProgram rng program

run_cases :: Int -> String -> IO String -> (String -> String -> Bool) -> IO Bool
run_cases n prog make_in check_out = do
    input <- liftM unlines $ mapM (\_ -> make_in) [1..n]
    putStrLn "RUNNING PROGRAM"
    results <- timeout (2*1000*1000) $ readProcessWithExitCode "java" [prog] input
    putStrLn "RAN PROGRAM"
    case results of
        Nothing -> return $ False
        Just (exit, out, err) -> case exit of
            ExitSuccess -> do
                return $ and $ map (uncurry check_out) $ zip (lines input) (lines out)
            ExitFailure _ -> return $ False

-- UTILITIES
rList :: Int -> Int -> Int -> IO [Int]
rList 0 _ _ = return $ []
rList n lo hi = do
    x <- randomRIO (lo, hi)
    lst <- rList (n-1) lo hi
    return $ x:lst

ints_to_spaces [] = ""
ints_to_spaces [x] = show x
ints_to_spaces (x:xs) = show x ++ " " ++ ints_to_spaces xs

spaces_to_ints str = map (\x -> read x :: Int) (words str)

sorted_list :: Int -> Int -> Int -> IO [Int]
sorted_list 0 _ _ = return $ []
sorted_list n lo hi = do
    incr <- randomRIO (0 :: Int, 2)
    x <- return $ min hi (lo + incr)
    lst <- sorted_list (n-1) x hi
    return $ x:lst

-- GCD
gcd_in :: IO String
gcd_in = do
    x <- randomRIO (0 :: Int, max_int)
    y <- randomRIO (0 :: Int, max_int)
    return $ ints_to_spaces [x,y]

gcd_check input output = (gcd x y) == d
    where
    [x,y] = spaces_to_ints input
    [d] = spaces_to_ints output

-- MOD INVERSE
mod_inv_in :: IO String
mod_inv_in = do
    x <- randomRIO (1 :: Int, 100)
    y <- randomRIO (2 :: Int, 100)
    if gcd x y == 1
    then return $ ints_to_spaces [x,y]
    else mod_inv_in

mod_inv_check :: String -> String -> Bool
mod_inv_check input output = correct == ans
    where
    [x,y] = spaces_to_ints input
    [ans] = spaces_to_ints output
    correct = mod_inv x y 1

--mod_inv base mod n | trace (show n ++ " " ++ show base ++ " " ++ show mod) False = undefined
mod_inv base mod n =
    if n*base `rem` mod == 1
    then n
    else mod_inv base mod (n+1)

-- MAX WEIGHT SUBSET
mw_in :: IO String
mw_in = do
    n <- randomRIO (1 :: Int, max_array_size)
    lst <- rList (n+1) 1 (1000*1000)
    return $ ints_to_spaces lst

mw_check input output = correct == ans
    where
    xs = spaces_to_ints input
    wts = init xs
    bound = last xs
    [ans] = spaces_to_ints output
    correct = mw_subset wts bound

mw_subset [] bound = if bound<0 then (minBound::Int) else 0
mw_subset (x:xs) bound = max
    (mw_subset xs bound)
    (x + (mw_subset xs (bound-x)))

-- BINARY SEARCH
binary_search_in :: IO String
binary_search_in = do
    n <- randomRIO (1 :: Int, 50)
    lst <- sorted_list n 1 30
    x <- randomRIO (1 :: Int, 40)
    return $ ints_to_spaces $ lst ++ [x]

binary_search_check input output = correct == ans
    where
    xs = spaces_to_ints input
    arr = init xs
    target = last xs
    [ans] = spaces_to_ints output
    correct = linear_search arr target 0

linear_search :: [Int] -> Int -> Int -> Int
linear_search [] _ _ = -1
linear_search (x:xs) target i =
    if x==target
    then i
    else linear_search xs target (i+1)

--NESTED PARENS
nested_parens_in = do
    n <- randomRIO (1 :: Int, 10)
    parens n

parens :: Int -> IO String
parens 0 = return $ ""
parens n = do
    b <- randomIO
    x <- return $ if b then '(' else ')'
    rest <- parens (n-1)
    return $ (x:rest)

nested_parens_check input output = correct == ans
    where
    correct = check_nested input 0
    [ans] = spaces_to_ints output

check_nested [] 0 = 1
check_nested [] _ = 0
check_nested _ n | n < 0 = 0
check_nested ('(' : xs) n = check_nested xs (n+1)
check_nested (')' : xs) n = check_nested xs (n-1)

--ADD
add_in = do
    x <- randomRIO (1 :: Int, 50)
    y <- randomRIO (1 :: Int, 50)
    return $ ints_to_spaces [x,y]

add_check input output = correct == ans
    where
    [x,y] = spaces_to_ints input
    [ans] = spaces_to_ints output
    correct = x+y

--NEXT PAL
next_pal_in = do
    n <- randomRIO (1 :: Int, 2)
    lst <- rList n 1 9
    mid <- randomIO
    midx <- randomRIO (1,9)
    final_lst <- return $ lst ++ (if mid then [midx] else []) ++ (reverse lst)
    return $ ints_to_spaces final_lst

next_pal_check input output = correct == ans
    where
    n = to_integer (spaces_to_ints input) 0
    correct = next_pal (n+1)
    ans = to_integer (spaces_to_ints output) 0

    to_integer [] a = a
    to_integer (x:xs) a = to_integer xs (10*a+x)

    is_pal n = (reverse strn) == strn
        where
        strn = show n

    next_pal n = if (is_pal n) then n else next_pal (n+1)
