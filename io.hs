import Control.Monad
import Data.Char
import System.IO
import System.Directory
import System.Environment  
import Data.List 

printIt = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            printIt

reverseWords :: String -> String
reverseWords s = unwords (map reverse (words s))
-- reverseWords = unwords . map reverse . words

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs

printUntilSpace = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            printUntilSpace  
        else return ()
        
printUntilSpace' = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        printUntilSpace'

getLines n = do
    res <- sequence $ take n $ repeat getLine
    print res
        
-- mapM <- map (Monad) and then sequence
capslockify = forever $ do
    contents <- getContents
    putStr $ map toUpper contents

onlyShort :: String -> String
onlyShort input = unlines shortLines
    where allLines = lines input
          shortLines = filter (\line -> length line < 10) allLines

--   main = interact $ unlines . filter ((<10) . length) . lines  
-- main = interact onlyShort

readIt path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

readIt' path = do
    withFile' path ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

readIt'' = do 
    contents <- readFile "text1.txt"
    putStr contents

writeIt :: FilePath -> FilePath -> IO ()
writeIt input output = do
    contents <- readFile input
    writeFile output $ map toUpper contents

addItem path = do
    item <- getLine
    appendFile path $ item ++ "\n"

removeItem path = do
    handle <- openFile path ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todos = lines contents
        numberedTodos = zipWith (\n line -> show n ++ " - " ++ line) [0..] todos
    putStrLn "Todos: "
    putStr $ unlines numberedTodos
    putStrLn "Choose one to delete"
    numberStr <- getLine
    let number = read numberStr
        newTodos = delete (todos !! number) todos
    hPutStr tempHandle $ unlines newTodos
    hClose handle
    hClose tempHandle
    removeFile path
    renameFile tempName path

getName :: IO ()
getName = do
    args <- getArgs
    let argsLength = length args
    progName <- getProgName
    putStr "The arguments are:"
    mapM putStr $ zipWith (\n arg -> " " ++ arg ++ if (argsLength - 1) == n then "\n" else ",") [0..] args   
    putStrLn $ "The program name is: " ++ progName  

main = getName