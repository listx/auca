\section{auca.lhs}

\begin{code}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import "monads-tf" Control.Monad.State
import Data.List (nub)
import System.IO
import System.Directory
import System.Environment
import System.Exit
import System.INotify

import AUCA.Core
import AUCA.Option
import AUCA.Util
\end{code}

\ct{main} checks for various errors before passing control over to \ct{prog}.

\begin{code}
main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stderr NoBuffering
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
	prog opts filesMaster
\end{code}

\ct{argsCheck} rejects any obviously illegal arguments.

\begin{code}
argsCheck :: Opts -> IO Int
argsCheck Opts{..}
	| null commands && null command_simple
		= errMsgNum "--command or --command-simple must be defined" 1
	| null file && null list
		= errMsgNum "either --file or --list must be defined" 1
	| otherwise = return 0
\end{code}

\ct{filesCheck} makes sure that all files defined by the user actually exist in the filesystem.

\begin{code}
-- Verify that the --file and --list arguments actually make sense.
filesCheck :: [Bool] -> [Bool] -> IO Int
filesCheck fs flist
	| any (==False) fs
		= errMsgNum "an argument to --file does not exist" 1
	| any (==False) flist
		= errMsgNum "a file defined in --list does not exist" 1
	| otherwise = return 0
\end{code}

\ct{prog} initializes the \ct{inotify} API provided by the Linux kernel.
We simply tell the API to check for any file modifications on the list of files in \ct{filesToWatch}, with the \ct{addWD} helper function defined in \ct{AUCA.Core}.
We then move on and enter into \ct{keyHandler}, a simple loop that checks for manual key presses by the user.
The calls to disable buffering on STDIN allow \ct{keyHandler} to detect individual key presses at a time.

\begin{code}
prog :: Opts -> [FilePath] -> IO ()
prog Opts{..} filesToWatch = do
	let
		comDef = if null command_simple
			then (head commands)
			else command_simple ++ " " ++ (head filesToWatch)
		commandSet = if null command_simple
			then commands
			else [command_simple ++ " " ++ (head filesToWatch)]
		tb = TimeBuffer
			{ bufSeconds = fromIntegral buffer_seconds
			, bufSecStockpile = 0
			}
	comset <- varCreate commandSet
	inotify <- initINotify
	putStrLn "\nFiles to watch:\n"
	mapM_ putStrLn filesToWatch
	mapM_ (\f -> addWD inotify f (eventHandler comset f inotify)) filesToWatch
	hSetBuffering stdin NoBuffering
	hSetEcho stdin False -- disable terminal echo
	let
		appState = AppState
			{ timeBuffer = tb
			, comSet = comset
			, inotify = inotify
			}
	helpMsg comset
	evalStateT keyHandler appState
\end{code}
