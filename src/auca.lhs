\section{auca.lhs}

\begin{code}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import Data.List (nub)
import System.IO
import System.Directory
import System.Environment
import System.Exit
import System.INotify

import AUCA.Core
import AUCA.Option
import AUCA.Util

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stderr NoBuffering
	hSetBuffering stdin NoBuffering
	hSetEcho stdin False -- disable terminal echo
	args' <- getArgs
	opts@Opts{..} <- (if null args' then withArgs ["--help"] else id) $ getOpts
	errNo <- argsCheck opts
	when (errNo > 0) $ exitWith $ ExitFailure errNo
	files <- if null list
		then return []
		else return . nub . filter (not . null) . lines =<< readFile list
	fs <- mapM doesFileExist file -- e.g., --file x --file y --file z
	-- e.g., --list x (and files defined in file x)
	flist <- mapM doesFileExist files
	errNo' <- filesCheck fs flist
	when (errNo' > 0) $ exitWith $ ExitFailure errNo
	let filesMaster = nub $ file ++ files
	helpMsg opts (head filesMaster)
	prog opts filesMaster

argsCheck :: Opts -> IO Int
argsCheck Opts{..}
	| null command && null command_simple = errMsgNum "--command or --command-simple must be defined" 1
	| null file && null list = errMsgNum "either --file or --list must be defined" 1
	| otherwise = return 0

-- Verify that the --file and --list arguments actually make sense.
filesCheck :: [Bool] -> [Bool] -> IO Int
filesCheck fs flist
	| any (==False) fs = errMsgNum "an argument to --file does not exist" 1
	| any (==False) flist = errMsgNum "a file defined in --list does not exist" 1
	| otherwise = return 0

prog :: Opts -> [FilePath] -> IO ()
prog opts@Opts{..} filesToWatch = do
	let comDef = if null command_simple
		then (head command)
		else command_simple ++ " " ++ (head filesToWatch)
	inotify <- initINotify
	putStrLn "\nFiles to watch:\n"
	mapM_ putStrLn filesToWatch
	wds <- mapM (\f -> addWatch inotify [Modify] f (eventHandler comDef)) filesToWatch
	keyHandler opts comDef (head filesToWatch) wds -- loop to handle key presses
\end{code}
