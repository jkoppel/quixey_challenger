

main :: IO ()
main = test "GCD.java"

test :: String -> IO ()
test program = do
    createProcess $ shell $ "javac " ++ program
