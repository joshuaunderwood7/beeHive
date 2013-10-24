module Hed where
import System.IO (hFlush, stdout)
import StringUtil
import FileMan
import Data.Char (toUpper)
import Control.Exception 

{- the structure of the document is 
 -  [[privious lines]] and these are reversed
 -  [the current line] but this will be only one string 
 -  [[the lines after current line]] 
-}

{- and the current line number -}

{- so the document structure is:-}
data Document = Buffer Int [[Char]] [Char] [[Char]]
    deriving (Show)

fillDocument :: String -> Document
fillDocument input = Buffer 1 [] (head readin) (tail readin)
    where readin = lines input

insertLine :: Document -> [Char] -> Document
insertLine (Buffer lineNum prev cur post) newline = 
                Buffer (lineNum + 1) (cur : prev) newline post

deleteLine :: Document -> Document
deleteLine (Buffer 0 _  _ _ )   = Buffer 0 [] [] []
deleteLine (Buffer _ [] _ post) = Buffer 1 [] (head post) (tail post)
deleteLine (Buffer lineNum prev _ post) = 
                   Buffer (lineNum - 1) (tail prev)  (head prev) post

replaceLine :: Document -> String -> Document
replaceLine (Buffer lineNum prev cur post) newline =
                                Buffer lineNum prev newline post

replaceSubLine :: Document -> [Char] -> [Char] -> Document
replaceSubLine (Buffer lineNum prev cur post) replace withThis = 
    Buffer lineNum prev (StringUtil.replaceFirstSubString replace withThis cur) post 

gotoLine :: Document -> Int -> Document
gotoLine org@(Buffer lineNum prev cur post) newline
    | newline < 1       = gotoLine org 1
    | lineNum > newline = gotoLine (Buffer (lineNum-1) (tail prev) (head prev) (cur:post)) newline  
    | post == []         = org
    | lineNum < newline = gotoLine (Buffer (lineNum+1) (cur:prev) (head post) (tail post)) newline  
    | otherwise         = org

allToString :: Document -> String
allToString (Buffer 1 _ cur post) = unlines (cur:post) 
allToString doc = allToString $ gotoLine doc 1

partToString :: Document -> Int -> Int -> String
partToString doc@(Buffer lineNum _ cur post) bgnng ndng
    | bgnng < 1        = partToString (gotoLine doc 1) 1 ndng  
    | bgnng > ndng     = partToString (gotoLine doc ndng) ndng bgnng 
    | lineNum == bgnng = unlines $ cur : take (ndng-bgnng) post
    | otherwise        = partToString (gotoLine doc bgnng) bgnng ndng

executeCommand :: [Char] -> Document -> Document
executeCommand [] doc = doc 
executeCommand command doc 
    | abriv == 'd'              = deleteLine doc
    | abriv == 'g' && args == 1 = gotoLine doc (((read.head.(drop 1).words) command)::Int )
    | abriv == 'r' && args >= 1 = replaceLine doc stament
    | abriv == 'i' && args >= 1 = insertLine doc stament
    | abriv == 's' && args == 2 = replaceSubLine doc ((head.words) command) ((head.(drop 1).words) command)
    | otherwise    = doc
        where abriv   = (head.head.words) command
              stament = ((tail.dropWhile (/=' ')).tail) command
              args    = length.tail.words $ command

promptLine doc@(Buffer lineNum _ _ _) =  do 
    putStr $ (show lineNum)  ++ " > "
    hFlush stdout
    getLine

progLoop :: IO Document -> IO Document 
progLoop input = do
    command <- input >>= promptLine
    if command /= "" then 
        case (head command) of
            'Q' -> input
            'V' -> input >>= (putStrLn.allToString) >> progLoop input
            'v' ->  catch ( do
                doc <-input
                let ltop = ((read.head.(drop 1).words) command) :: Int 
                let lbot = ((read.head.(drop 2).words) command) :: Int 
                putStrLn $ partToString doc ltop lbot 
                progLoop input ) (handleError (progLoop input)) 
            _   -> do
                doc <- input 
                catch ( progLoop $ return $ executeCommand command doc) 
                      ( handleError (progLoop input) ) 
    else progLoop input

handleError  :: IO a -> SomeException -> IO a
handleError callback exception = (print.show) exception >> callback

main = do
    let a = []
    {-
    let a = Buffer 4 ["3","2","1"] "4" ["5","6","7"]
    let b =  replaceSubLine (insertLine a "Pies") "ie" "zz"
    print a
    print b
    putStrLn $ partToString b (-20) 20
    putStrLn $ allToString (insertLine a "Pies") 
    putStrLn $ allToString b 
    -}
    progLoop $ readFileToBuffer "test.txt"  fillDocument
    --print "saving test.txt"
    --(writeBufferToFile "test.txt").allToString --The document--
    print "bye"
