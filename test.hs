import System.Process
import System.IO
import System.Random
import System.Exit
import Control.Monad
import Debug.Trace
import Data.Int
import Data.List.Split
import System.Timeout

challenges = [("GCD", gcd_in, gcd_check),
              ("MAX_SUBSET", mw_in, mw_check),
              ("MOD_INVERSE", mod_inv_in, mod_inv_check),
              ("BINARY_SEARCH", binary_search_in, binary_search_check),
              ("NESTED_PARENS", nested_parens_in, nested_parens_check)
             ]

test_cases = 100
max_array_size = 6
max_int = 1000*1000

-- MAIN CODE
main = do
    test 0
    test 1
    test 2
    test 3
    test 4

fix :: Int -> IO ()
fix n = do
    

    p <- runCommand ("javac " ++ file)
    waitForProcess p
    good <- run_cases test_cases name input output
    putStrLn $ show good
    where
        (name, input, output) = challenges !! n
        file = name ++ ".java"

run_cases :: Int -> String -> IO String -> (String -> String -> Bool) -> IO Bool
run_cases n prog make_in check_out = do
    input <- liftM unlines $ mapM (\_ -> make_in) [1..n]
    results <- timeout (1000*1000) $ readProcessWithExitCode "java" [prog] input
    case results of
        Nothing -> return $ False
        Just (exit, out, err) -> trace (show input ++ " ==> " ++ show out) (case exit of
            ExitSuccess -> do
                return $ and $ map (uncurry check_out) $ zip (lines input) (lines out)
            ExitFailure _ -> trace (err++" "++input) (return $ False))

-- UTILITIES
rList :: Int -> IO [Int]
rList 0 = return $ []
rList n = do
    x <- randomRIO (1 :: Int, max_int)
    lst <- rList (n-1)
    return $ x:lst

ints_to_spaces [] = ""
ints_to_spaces [x] = show x
ints_to_spaces (x:xs) = show x ++ " " ++ ints_to_spaces xs

spaces_to_ints str = map (\x -> read x :: Int) (splitOn " " str)

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
    lst <- rList (n+1)
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
