import Data.List (elemIndex)

findFirstInSorted :: [Int] -> Int -> Int
findFirstInSorted array x = case elemIndex x array of
                                        Just index -> index
                                        Nothing    -> -1
