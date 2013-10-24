module StringUtil where
import Data.List

stringComp :: [Char] -> [Char] -> Bool
stringComp [] [] = True
stringComp [] _  = False
stringComp _  [] = False
stringComp (x:xs) (y:ys)
    | x /= y    = False
    | otherwise = stringComp xs ys


subStringsOfLength :: Int -> [Char] -> [[Char]]
subStringsOfLength _ [] = []
subStringsOfLength len target@(x:xs) = take len target : subStringsOfLength len xs

replaceSubString :: [Char] -> [Char] -> [Char] -> [Char]
replaceSubString _ _ [] = []
replaceSubString pattern withThis s@(x:xs) 
    | pattern `isPrefixOf` s = withThis ++ replaceSubString pattern withThis (drop (length pattern) s)
    | otherwise              = x : replaceSubString pattern withThis xs


replaceSubString :: [Char] -> [Char] -> [Char] -> [Char]
replaceSubString pattern withThis s@(x:xs) 
    | pattern `isPrefixOf` s = withThis ++ replaceSubString pattern withThis (drop (length pattern) s)
    | otherwise              = x : replaceSubString pattern withThis xs
