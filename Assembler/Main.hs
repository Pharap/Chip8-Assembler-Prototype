--import Chip8.Assembly.Language
import Chip8.Assembly.Parsing
import Chip8.Assembly.Compiling
import Chip8.Assembly.Compiling.State

import System.Environment (getArgs)

import Text.Parsec hiding (label)

import qualified Data.ByteString.Lazy as ByteString

import Control.Monad

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [] -> print "No files specified"
        _ -> void $ mapM processFile arguments

processFile :: FilePath -> IO ()
processFile path =
    compileFile path (changeExtension path "C8")
    where
        dropExtension :: FilePath -> FilePath
        dropExtension =
            takeWhile (/= '.')

        changeExtension :: FilePath -> String -> FilePath
        changeExtension path extension =
            dropExtension path ++ '.':extension

compileFile :: FilePath -> FilePath -> IO ()
compileFile source destination = do
    text <- readFile source
    case parse file source text of
        Left error ->
            print error
        Right value -> do
            let Result bytes diagnostics = compile value
            sequence_ $ map print $ diagnostics
            case getErrors diagnostics of
                [] ->
                    ByteString.writeFile destination bytes
                _ ->
                    return ()