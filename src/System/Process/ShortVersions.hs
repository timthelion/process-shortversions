{-# LANGUAGE PackageImports #-}
{-
This module provides several shortened versions of functions from System.Process

GPL3. License info is at the bottom of the file.
-}
module System.Process.ShortVersions
 (runCommandInDir
 ,commandExists) where

import "process" System.Process
 (createProcess
 ,proc
 ,cwd
 ,waitForProcess
 ,readProcessWithExitCode)

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