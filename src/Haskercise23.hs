-----------------------------------------------------------------------------------
-- |
-- Module      : Haskercise23
-- Copyright   :
-- License     :
-- Maintainer  : Sampath Singamsetty <Singamsetty.Sampath@gmail.com>
-- Stability   :
-- Portability : Haskell 2010
-- Description : IO
--

module Haskercise23 where

import           Control.Exception    (finally)
import qualified Data.ByteString.Lazy as BL
import           System.Directory     (getTemporaryDirectory, removeFile)
import           System.IO
import           System.IO.Error      (catchIOError)
-----------------------------------------------------------------------------------
-- | Working with a temp file
temp :: IO ()
temp = withTempFile "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempFile tempHandle = do putStrLn "Welcome to tempFile.hs"
                                  putStrLn $ "Temporary file available at " ++ tempFile
                                  pos <- hTell tempHandle
                                  putStrLn $ "Initial position of file at " ++ show pos
                                  let tempData = show [1 .. 10]
                                  putStrLn $ "Writing one line at a time " ++
                                             show (length tempData) ++ " bytes: " ++
                                             tempData
                                  hPutStrLn tempHandle tempData
                                  pos <- hTell tempHandle
                                  putStrLn $ "New position after writing: " ++ show pos
                                  putStrLn "The file content is: "
                                  hSeek tempHandle AbsoluteSeek 0
                                  c <- hGetContents tempHandle
                                  putStrLn c
                                  putStrLn "Expressing as haskell literal"
                                  print c

withTempFile :: String -> (FilePath -> Handle -> IO ()) -> IO ()
withTempFile pattern fun =
  do tempDir <- catchIOError getTemporaryDirectory (\_ -> return ".")
     (tempFile, tempHandler) <- openTempFile tempDir pattern
     finally (fun tempFile tempHandler) (do hClose tempHandler
                                            removeFile tempFile)

-- | Check if a file is an ELF object file (exe under posix)
--   A Byte sequence which identifies the file type is Magic Number
--   The function compares first 4 butes of ByteString to a magic number
hasElfMagic :: BL.ByteString -> Bool
hasElfMagic content = BL.take 4 content == elfMagic
    where
        elfMagic = BL.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
    content <- BL.readFile path
    return $ hasElfMagic content

-- | Filename matching with glob patterns
--
