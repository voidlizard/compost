module Main where

import System.Environment
import System.IO
import Text.Printf
import System.FilePath
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Exception
import System.Path.Glob
import System.Posix.Files
import System.Posix.Types
import qualified System.Directory as D
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Crypto.Hash.SHA1 as H

import Magic.Data (MagicFlag(..))
import Magic.Init
import Magic.Operations

type FileHash = String
data Conf = Conf { path :: FilePath }
data ObjectInfo = ObjectInfo { objectSize :: FileOffset, objectHash :: FileHash, objectMimeType :: String }

fileHash :: FilePath -> IO FileHash
fileHash f = do
    content <- L.readFile f
    let hash = H.hashlazy content
    return $ concatMap (printf "%02x") (B.unpack hash)

objectRead :: Conf -> FileHash -> IO (Maybe L.ByteString)
objectRead conf hash = do
    let p = objectPath conf hash
    handle handler $ do 
        bytes <- L.readFile p
        return $ Just bytes
    where handler (SomeException e) = return Nothing

objectPath conf hash =
    let (prefix, name) = splitAt 2 hash
    in joinPath [(path conf), prefix, name]

objectMime magic conf hash = do
    let fn = objectPath conf hash
    exists <- D.doesFileExist fn
    if not exists 
        then return Nothing
        else magicFile magic fn >>= return . Just

objectExists conf hash = do
    D.doesFileExist $ objectPath conf hash

objectFormatInfo (ObjectInfo {objectSize = sz, objectHash = hash, objectMimeType = mime}) = 
    printf "%s\t%12s\t%s" hash (show sz) mime :: String

objectListAll magic conf = do
    let mask = (path conf) ++ "/*/**"
    files <- glob mask >>= filterM (fmap isRegularFile . getFileStatus)
    mapM withFile files
    where withFile f = do
            let hash = concat $ reverse $ take 2 $ reverse $ splitDirectories f
            mime <- objectMime magic conf hash
            size <- fmap fileSize $ getFileStatus f
            return $ ObjectInfo size hash (fromMaybe "" mime)

main = do
    args <- getArgs
    magic <- magicOpen [MagicMime]
    magicLoadDefault magic
    let conf = Conf { path = "test" }
    case args of
        ["hash", x] -> do q <- fileHash x
                          putStrLn $ "Hash: " ++ q
                          putStrLn $ "Path: " ++ objectPath conf q
        ["exists", x] -> do q <- fileHash x
                            e <- objectExists conf q 
                            putStrLn $ if e then "yes" else "no"
        ["put", x]    -> do q <- fileHash x
                            let fn = objectPath conf q
                            D.createDirectoryIfMissing True $ fst $ splitFileName fn 
                            D.copyFile x fn
        ["mime", x]   -> do m <- objectMime magic conf x
                            when (isJust m) $ putStrLn $ fromJust m 
        ["get", x]    -> do bytes <- objectRead conf x 
                            when (isJust bytes) $ L.hPutStr stdout $ fromJust bytes
        ["get", x, out] -> do D.copyFile (objectPath conf x) out
        ["list"]        -> do files <- objectListAll magic conf
                              mapM_ (putStrLn . objectFormatInfo) files
 
        x:_         -> putStrLn $ "Unknown command: " ++ x
        []          -> putStrLn $ "Usage: hdump hash|exists|put|get|mime... filename|object-hash"

