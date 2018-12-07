import Data.Aeson
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix (joinPath, takeBaseName)

import Language.Python.Common.AST
import Language.Python.Common.Token
import Language.Python.Version2

main :: IO ()
main =  do
  args <- getArgs
  case args of
    (src:dest:[]) ->
      convertFiles src dest
    _ ->
      putStrLn "Pass 2 args: source and target dirs"

convertFiles :: String -> String -> IO ()
convertFiles src dest = do
  pyFiles <- listDirectory src
  traverse (\f -> convertFile f dest) $ preparePyFiles pyFiles
  putStrLn $ "Will convert " ++ src ++ " and " ++ dest
  where
    endsWithPy f = any (`isSuffixOf` f) [".py"]
    doesntStartWithUnderscores f = all (\prefix -> not $ prefix `isPrefixOf` f) ["__"]
    preparePyFiles files =
      fmap (\f -> joinPath [src, f]) $ filter (\f -> endsWithPy f && doesntStartWithUnderscores f) files

convertFile :: String -> String -> IO ()
convertFile fileName dest = do
  fileStr <- readFile fileName
  let modObj = parseModule fileStr fileName
  either (putStr . show) writeJsonFile modObj
  where
    writeJsonFile :: (ModuleSpan, [Token]) -> IO ()
    writeJsonFile ast =
      let
        json = T.decodeUtf8 . encode $ ast
        destFileName = (takeBaseName fileName) ++ ".json"
      in
        T.writeFile (joinPath [dest, destFileName]) json

