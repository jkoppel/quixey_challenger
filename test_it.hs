import System.Process
import System.IO
import System.Random
import System.Exit
import Control.Monad
import Debug.Trace
import Data.Int
import Data.List.Split

challenges = [("GCD", gcd_in, gcd_check),
              ("MAX_SUBSET", mw_in, mw_check),
              ("MOD_INVERSE", mod_inv_in, mod_inv_check)
             ]

test_cases = 10
max_array_size = 10
max_int = 1000*1000

-- MAIN CODE
main = test 2

test :: Int -> IO ()
test n = do
    p <- runCommand ("javac " ++ file)
    waitForProcess p
    good <- run_cases test_cases name input output
    putStrLn $ show good
    where
        (name, input, output) = challenges !! n
        file = name ++ ".java"

run_cases :: Int -> String -> IO String -> (String -> String -> Bool) -> IO Bool
run_cases 0 _ _ _ = return $ True
run_cases n prog make_in check_out = do
    input <- make_in
    (exit, out, err) <- readProcessWithExitCode "java" [prog] input
    case exit of
        ExitSuccess -> do
            if check_out input out
            then run_cases (n-1) prog make_in check_out
            else trace (input ++ " ==> " ++ out) (return False)
        ExitFailure _ -> trace (err++" "++input) (return $ False)

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

-- GCD
gcd_in :: IO String
gcd_in = do
    x <- randomRIO (0 :: Int, max_int)
    y <- randomRIO (0 :: Int, max_int)
    return $ ((show x) ++ " " ++ (show y))

gcd_check input output = (gcd x y) == d
    where
    [x,y] = spaces_to_ints input
    [d] = spaces_to_ints output

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

-- MOD INVERSE
mod_inv_in :: IO String
mod_inv_in = do
    x <- randomRIO (1 :: Int, 1000)
    y <- randomRIO (1 :: Int, 1000)
    if gcd x y == 1
    then return $ ints_to_spaces [x,y]
    else mod_inv_in

mod_inv_check :: String -> String -> Bool
mod_inv_check input output = trace (show correct ++ " " ++ show ans) (correct == ans)
    where
    [x,y] = spaces_to_ints input
    [ans] = spaces_to_ints output
    correct = mod_inv x y 1

mod_inv base mod n =
    if n*base `rem` mod == 1
    then n
    else mod_inv base mod (n+1)

-- BINARY SEARCH
