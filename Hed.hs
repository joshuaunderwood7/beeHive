module Hed where
import System.IO 
import System.Environment 
import StringUtil
import FileMan
import Data.Char (toUpper)
import Data.Maybe
import Control.Exception 

{- the structure of the document is 
 -  [[privious lines]] and these are reversed
 -  [the current line] but this will be only one string 
 -  [[the lines after current line]] 
-}

{- and the current line number -}

{- so the document structure is:-}
data Line = LineBuffer Int [Char] Char [Char]
    deriving (Show)

data Document = Buffer Int [[Char]] [Char] [[Char]]
    deriving (Show)

fillDocument :: String -> Document
fillDocument input = Buffer 1 [] (head readin) (tail readin)
    where readin = lines input

appendLine :: Document -> [Char] -> Document
appendLine (Buffer 0 [] [] []) newline = Buffer 1 [] newline [] 
appendLine (Buffer lineNum prev cur post) newline = 
                Buffer (lineNum + 1) (cur : prev) newline post

insertLine :: Document -> [Char] -> Document
insertLine (Buffer 0 [] [] []) newline = Buffer 1 [] newline [] 
insertLine (Buffer lineNum prev cur post) newline = 
                Buffer lineNum prev newline (cur : post)

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
    | abriv == 'a' && args >= 1 = appendLine doc stament
    | abriv == 's' && args == 2 = replaceSubLine doc ((head.words) stament) ((head.(drop 1).words) stament)
    | otherwise    = doc
        where abriv   = (head.head.words) command
              stament = ((tail.dropWhile (/=' ')).tail) command
              args    = length.tail.words $ command

getLineNum :: Document -> Int
getLineNum (Buffer lineNum _ _ _) = lineNum   

promptLine doc@(Buffer lineNum _ _ _) =  do 
    putStr $ (show lineNum)  ++ " > "
    hFlush stdout
    getLine

insertMode :: IO Document -> IO Document
insertMode input = do
    line <- putStr "I " >> input >>= promptLine
    case line of 
        ".." -> input
        _    -> do
            doc <- input
            insertMode $ return $ appendLine doc line

visualMode :: IO Document -> IO Document
visualMode input = do
    doc <- input
    let line = getLineNum doc
    hSetBuffering stdin NoBuffering
    putStr $ "V " ++ (show line) ++ " > "
    putStr $partToString doc line line   
    hFlush stdout
    hSetEcho stdin False
    command <- getChar
    hSetEcho stdin True
    case (command) of
        'j' -> return (executeCommand ("g " ++ (show (line + 1))) doc) >>= visualMode.return
        'k' -> return (executeCommand ("g " ++ (show (line - 1))) doc) >>= visualMode.return
        'q' -> do 
            hSetBuffering stdin LineBuffering 
            input
        _   -> putStrLn "" >> visualMode input


progLoop :: IO [Document] -> IO Document 
progLoop input = do
    command <- input >>= promptLine.head
    if command /= "" then 
        case (head command) of
            'Q' -> fmap head input
            'V' -> input >>= (putStrLn.allToString.head) >> progLoop input
            'v' ->  catch ( do
                doc <-input
                let ltop = ((read.head.(drop 1).words) command) :: Int 
                let lbot = ((read.head.(drop 2).words) command) :: Int 
                putStrLn $ partToString (head doc) ltop lbot 
                progLoop input ) (handleError (progLoop input)) 
            '.' -> do { doc <- input; insertMode (fmap head input) >>= (\x -> (progLoop.return) $ x:doc) }
            'n' -> do { doc <- input; visualMode (fmap head input) >>= (\x -> (progLoop.return) $ x:doc) }
            'u' -> do 
                doc <- input 
                if ((length doc) > 1) then progLoop $ fmap tail input else progLoop input
            _   -> do
                doc <- input 
                catch ( progLoop $ return $ (executeCommand command (head doc) : doc ) )  
                      ( handleError (progLoop input) ) 
    else progLoop input

handleError  :: IO a -> SomeException -> IO a
handleError callback exception = (print.show) exception >> callback

getFilename :: [a] ->  Maybe a
getFilename []    = Nothing
getFilename (x:_) = Just x

getFilename' :: [String] ->  String
getFilename' []    = "test.txt" 
getFilename' (x:_) = x 

textInput :: String -> IO String
textInput x = do
    input <- getChar
    if (input == '\n') 
        then return $ reverse x
    else do 
        print $ reverse x
        textInput  (input : x)

main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdin LineBuffering

    fileName <- getArgs >>= return.getFilename'
    print fileName

    progLoop $ readFileToBuffer fileName fillDocument >>= (\x -> return (x:[]))

    --print "saving test.txt"
    --(writeBufferToFile "test.txt").allToString --The document--

    print "bye."

    --textInput ""

--Now this is the proper way to implement ed in Haskell
--main = getLine >> putStrLn "?" >> main
