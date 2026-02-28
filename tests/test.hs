{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

{- HLINT ignore "Avoid restricted function" -}

#include "HsBaseConfig.h"

import Control.Concurrent (
  forkIO,
  killThread,
  newEmptyMVar,
  putMVar,
  readMVar,
  threadDelay,
 )
import Control.Exception (bracket, bracket_, finally)
import Control.Monad (replicateM_, unless)
import Data.List (group, sort)
import Data.String (fromString)
import System.Directory.OsPath (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  removeDirectoryRecursive,
  removeFile,
 )
import System.Environment (setEnv, unsetEnv)
import System.Exit (die)
import System.File.OsPath (readFile)
import System.IO (hClose, hIsClosed, hIsOpen, hPutStrLn)
import System.IO.Temp.OsPath (
  createTempFileName,
  emptySystemTempFile,
  emptyTempFile,
  getCanonicalTemporaryDirectory,
  openNewBinaryFile,
  withSystemTempDirectory,
  withSystemTempFile,
  writeSystemTempFile,
 )
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (
  OsPath,
  decodeFS,
  equalFilePath,
  isAbsolute,
  osp,
  takeBaseName,
  takeDirectory,
  takeFileName,
 )
import System.OsString (isPrefixOf, isSuffixOf)
import System.Posix.Types (FileMode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Prelude hiding (readFile)

#ifndef mingw32_HOST_OS
import Data.Bits ((.&.))
import Data.Functor (void)
import System.OsString.Internal.Types (getOsString)
import System.Posix.Files.PosixString (
  fileMode,
  getFileStatus,
  setFileCreationMask,
  )
#endif

#if defined(mingw32_HOST_OS) || !defined(HAVE_UMASK)
setFileCreationMask0 :: IO ()
setFileCreationMask0 = pure ()
#else
setFileCreationMask0 :: IO ()
setFileCreationMask0 = void $ setFileCreationMask 0
#endif

#if defined(mingw32_HOST_OS) || !defined(HAVE_UMASK)
checkFileStatus :: OsPath -> FileMode -> Assertion
checkFileStatus = const $ const $ pure ()
#else
checkFileStatus :: OsPath -> FileMode -> Assertion
checkFileStatus fp expected = do
  status <- getFileStatus (getOsString fp)
  fileMode status .&. 0o777  @?= expected
#endif

#if defined(mingw32_HOST_OS) && __GLASGOW_HASKELL__ < 900
oldGhcOnWin :: Bool
oldGhcOnWin = True
#else
oldGhcOnWin :: Bool
oldGhcOnWin = False
#endif

#if defined(netbsd_HOST_OS)
-- NetBSD has issues with clock_getres, which is used by openTempFile from file-io
ignoreOnNetBSD :: Assertion -> Assertion
ignoreOnNetBSD = const $ pure ()
#else
ignoreOnNetBSD :: Assertion -> Assertion
ignoreOnNetBSD = id
#endif

main :: IO ()
main = do
  -- force single-thread execution, because changing TMPDIR in one of the
  -- tests may leak to the other tests
  setEnv "TASTY_NUM_THREADS" "1"
  setFileCreationMask0
  sysTmpDir <- getCanonicalTemporaryDirectory
  putStrLn $ "getCanonicalTemporaryDirectory = " ++ show sysTmpDir
  createDirectoryIfMissing True sysTmpDir
  doesSysTmpDirExist <- doesDirectoryExist sysTmpDir
  unless doesSysTmpDirExist $
    die $
      show sysTmpDir ++ " does not exist"
  defaultMain $
    testGroup
      "Tests"
      [ testCase "openNewBinaryFile" $ ignoreOnNetBSD $ do
          (fp, fh) <- openNewBinaryFile sysTmpDir [osp|test.txt|]
          let fn = takeFileName fp
          assertBool ("Does not match template: " ++ decodeOsPath fn) $
            oldGhcOnWin || ([osp|test|] `isPrefixOf` fn) && ([osp|.txt|] `isSuffixOf` fn)
          assertBool (decodeOsPath fp ++ " is not in the right directory " ++ decodeOsPath sysTmpDir) $
            takeDirectory fp `equalFilePath` sysTmpDir
          hClose fh
          assertBool "File does not exist" =<< doesFileExist fp
          checkFileStatus fp 0o666
          removeFile fp
      , testCase "withSystemTempFile" $ ignoreOnNetBSD $ do
          (fp, fh) <- withSystemTempFile [osp|test.txt|] $ \fp fh -> do
            let fn = takeFileName fp
            assertBool ("Does not match template: " ++ decodeOsPath fn) $
              oldGhcOnWin || ([osp|test|] `isPrefixOf` fn) && ([osp|.txt|] `isSuffixOf` fn)
            assertBool (decodeOsPath fp ++ " is not in the right directory " ++ decodeOsPath sysTmpDir) $
              takeDirectory fp `equalFilePath` sysTmpDir
            assertBool "File not open" =<< hIsOpen fh
            hPutStrLn fh "hi"
            assertBool "File does not exist" =<< doesFileExist fp
            checkFileStatus fp 0o600
            return (fp, fh)
          assertBool "File still exists" . not =<< doesFileExist fp
          assertBool "File not closed" =<< hIsClosed fh
      , testCase "withSystemTempDirectory" $ do
          fp <- withSystemTempDirectory [osp|test.dir|] $ \fp -> do
            let fn = takeFileName fp
            assertBool ("Does not match template: " ++ decodeOsPath fn) $
              [osp|test.dir|] `isPrefixOf` fn
            assertBool (decodeOsPath fp ++ " is not in the right directory " ++ decodeOsPath sysTmpDir) $
              takeDirectory fp `equalFilePath` sysTmpDir
            assertBool "Directory does not exist" =<< doesDirectoryExist fp
            checkFileStatus fp 0o700
            pure fp
          assertBool "Directory still exists" . not =<< doesDirectoryExist fp
      , testCase "writeSystemTempFile" $ ignoreOnNetBSD $ do
          fp <- writeSystemTempFile [osp|blah.txt|] (fromString "hello")
          str <- readFile fp
          fromString "hello" @?= str
          removeFile fp
      , testCase "emptySystemTempFile" $ ignoreOnNetBSD $ do
          fp <- emptySystemTempFile [osp|empty.txt|]
          assertBool "File doesn't exist" =<< doesFileExist fp
          removeFile fp
      , testCase "withSystemTempFile returns absolute path" $ ignoreOnNetBSD $ do
          bracket_ (setEnv "TMPDIR" ".") (unsetEnv "TMPDIR") $ do
            withSystemTempFile [osp|temp.txt|] $ \fp _ ->
              assertBool "Not absolute" $ isAbsolute fp
      , testCase "withSystemTempDirectory is not interrupted" $ ignoreOnNetBSD $ do
          -- this mvar is both a channel to pass the name of the directory
          -- and a signal that we finished creating files and are ready
          -- to be killed
          mvar1 <- newEmptyMVar
          -- this mvar signals that the withSystemTempDirectory function
          -- returned and we can check whether the directory has survived
          mvar2 <- newEmptyMVar
          threadId <-
            forkIO $
              ( withSystemTempDirectory [osp|temp.test.|] $ \dir -> do
                  replicateM_ 100 $ emptyTempFile dir [osp|file.xyz|]
                  putMVar mvar1 dir
                  threadDelay 1000000
              )
                `finally` putMVar mvar2 ()
          dir <- readMVar mvar1
          -- start sending exceptions
          replicateM_ 10 $ forkIO $ killThread threadId
          -- wait for the thread to finish
          readMVar mvar2
          -- check whether the directory was successfully removed
          assertBool "Directory was not removed" . not =<< doesDirectoryExist dir
      , testCase "createTempFileName" $ do
          let template = [osp|testdir|]
              -- createTempFileName with some tests
              checkedCreateTempFileName = do
                fp <- createTempFileName sysTmpDir template
                let directParent = takeDirectory fp
                assertBool ("Parent directory " ++ decodeOsPath directParent ++ " does not match template: " ++ decodeOsPath template) $
                  template `isPrefixOf` takeBaseName directParent
                assertBool "Parent directory does not exist" =<< doesDirectoryExist directParent
                assertBool "File already exists" . not =<< doesFileExist fp
                pure fp

              -- Helper for the test just to ensure we don't leave garbage lying
              -- around when we run/abort tests.
              withTempFileName =
                bracket
                  checkedCreateTempFileName
                  (removeDirectoryRecursive . takeDirectory)

          -- Now make some filenames, check that they are all different. Note that
          -- we run the tests we defined above on each one. We don't want to plug
          -- a huge number here to not hit any file resource limits.
          let checkDifferent 0 fns =
                assertBool "Got duplicate temporary filenames"
                  . all ((== 1) . length)
                  . group
                  $ sort fns
              checkDifferent n fns = withTempFileName $ \fp ->
                checkDifferent (n - 1) (fp : fns)

          checkDifferent (50 :: Int) []
      ]

decodeOsPath :: OsPath -> String
decodeOsPath = unsafePerformIO . decodeFS
