{-# LANGUAGE PackageImports, CPP #-}
{-
This module provides several shortened versions of functions from System.Process

GPL3. License info is at the bottom of the file.
-}
module System.Process.ShortVersions
 (runCommandInDir
 ,commandExists
 ,runCommandWithExitCode
 ,readCreateProcessWithExitCode) where

import "process" System.Process
 (createProcess
 ,CreateProcess
 ,StdStream(CreatePipe)
 ,terminateProcess
 ,proc
 ,cwd
 ,waitForProcess
 ,readProcessWithExitCode
 ,std_in
 ,std_out
 ,std_err)

import qualified "base" Control.Exception as C
 (evaluate)
 
import "base" Control.Exception
 (SomeException
 ,mask
 ,try
 ,onException
 ,throwIO)

import "base" Control.Monad
 (unless)

import "base" System.IO
 (hPutStr
 ,hGetContents
 ,hFlush
 ,hClose)

import "base" Control.Concurrent
 (forkIO)

import "base" Control.Concurrent.MVar
 (takeMVar
 ,putMVar
 ,newEmptyMVar)

import "deepseq" Control.DeepSeq
 (rnf)

import "base" System.Exit
 (ExitCode(ExitSuccess))

-- | Runs a command with a given current working directory.
runCommandInDir
 :: FilePath -- ^ directoryToRunIn
 -> String   -- ^ command
 -> [String] -- ^ arguments
 -> IO ()
runCommandInDir pwd command args = do
 (_,_,_,ph)<-createProcess $ (proc command args){cwd = Just pwd}
 _ <- waitForProcess ph
 return ()

runCommandWithExitCode
 :: String -- ^ command
 -> [String] -- ^ arguments
 -> IO ExitCode
runCommandWithExitCode command args = do
 (exitCode,_,_) <- readProcessWithExitCode command args ""
 return exitCode

-- | TODO Don't use which? http://stackoverflow.com/questions/592620/check-if-a-program-exists-from-a-bash-script
commandExists
 :: String -- ^ The command(does not have to be the full path.)
 -> IO Bool -- ^ True if exists.
commandExists command = do
 exitCode <- runCommandWithExitCode "which" [command]
 case exitCode of
  ExitSuccess -> return True
  _ -> return False

{- Taken from the process package -}
{- |
@readCreateProcessWithExitCode@ creates an external process, reads its
standard output and standard error strictly, waits until the process
terminates, and then returns the 'ExitCode' of the process,
the standard output, and the standard error.

-}
readCreateProcessWithExitCode
    :: CreateProcess            -- ^ the process to run
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode process input =
    mask $ \restore -> do
      (Just inh, Just outh, Just errh, pid)
        <- createProcess
            process{ std_in  = CreatePipe,
                     std_out = CreatePipe,
                     std_err = CreatePipe }
      flip onException
        (do hClose inh; hClose outh; hClose errh;
            terminateProcess pid; waitForProcess pid) $ restore $ do
        -- fork off a thread to start consuming stdout
        out <- hGetContents outh
        waitOut <- forkWait $ C.evaluate $ rnf out

        -- fork off a thread to start consuming stderr
        err <- hGetContents errh
        waitErr <- forkWait $ C.evaluate $ rnf err

        -- now write and flush any input
        let writeInput = do
              unless (null input) $ do
                hPutStr inh input
                hFlush inh
              hClose inh
        writeInput

        -- wait on the output
        waitOut
        waitErr

        hClose outh
        hClose errh

        -- wait on the process
        ex <- waitForProcess pid

        return (ex, out, err)

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

{-
-- Copyright (C) 2013 Timothy Hobbs <timothyhobbs@seznam.cz>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}