\section{AUCA/Core.lhs}

There are two main functions here --- \ct{eventHandler} and \ct{keyHandler}.
\ct{eventHandler} hooks into the \ct{inotify} API for executing arbitrary commands, and \ct{keyHandler} handles all interactive key presses by the user.

\begin{code}
{-# LANGUAGE RecordWildCards #-}

module AUCA.Core where

import Data.Maybe
import System.Exit
import System.INotify
import System.Process

import AUCA.Option
import AUCA.Util

-- Only run the given command on a file modification event (ignore directory
-- modifications or paths that do not exist).
eventHandler :: String -> Event -> IO ()
eventHandler comDef Modified{..}
	| isDirectory || isJust maybeFilePath
		= putStrLn "Directory modification" >> return ()
	| otherwise = do
		putStrLn []
		showTime
		putStr $ ": " ++ colorize Magenta "change detected"
		putStrLn $ "; executing command " ++ squote (colorize Blue comDef)
		runCom $ cmd comDef
eventHandler _ ev = putStrLn ("Event: " ++ show ev) >> return ()
\end{code}

We only execute the given command when the detected event is a \textit{modification} event of a \ct{file}.
We ignore all other types of events, but print out info messages to tell the user what happened.

\begin{code}
keyHandler :: Opts -> String -> FilePath -> [WatchDescriptor] -> IO ()
keyHandler o@Opts{..} comDef f wds = keyHandler' =<< getChar
	where
	keyHandler' 'h' = helpMsg o f >> keyHandler o comDef f wds
	keyHandler' 'q' = putStrLn [] >> mapM_ removeWatch wds
	keyHandler' key = do
		if elem key comKeys
			then case lookup [key] comHash of
				Just com -> do
					putStrLn []
					showTime
					putStr $ ": "
						++ colorize Cyan "manual override"
						++ " (slot "
						++ colorize Yellow [key]
						++ ")"
					putStrLn $ "; executing command "
						++ squote (colorize Blue com)
					runCom $ cmd com
				_ -> do
					putStrLn []
					putStrLn $ "command slot for key "
						++ squote (colorize Yellow [key]) ++ " is empty"
			else do
				putStrLn []
				showTime
				putStr $ ": " ++ colorize Cyan "manual override"
				putStrLn $ "; executing command "
					++ squote (colorize Blue comDef)
				runCom $ cmd comDef
		keyHandler o comDef f wds
	comHash :: [(String, String)]
	comHash = if null command
		then [("1", command_simple ++ " " ++ f)]
		else zip (map show [(1::Int)..10]) command
	comKeys :: String
	comKeys = concatMap show [(0::Int)..9]
\end{code}

\ct{keyHandler} handles single key presses by the user.
The \ct{comHash} and \ct{comKeys} structures define the hotkeys available to the user if multiple commands were defined.

\begin{code}
runCom :: CreateProcess -> IO ()
runCom com = do
	(_, _, _, p) <- createProcess com
	exitStatus <- waitForProcess p
	showTime
	putStrLn $ ": " ++ if (exitStatus == ExitSuccess)
		then colorize Green "command executed successfully"
		else colorize Red "command failed"

cmd :: String -> CreateProcess
cmd com = CreateProcess
	{ cmdspec = ShellCommand $
		(com ++ " 2>&1 | sed \"s/^/  " ++ colorize Cyan ">" ++ " /\"")
	, cwd = Nothing
	, env = Nothing
	, std_in = CreatePipe
	, std_out = Inherit
	, std_err = Inherit
	, close_fds = True
	, create_group = False
	}

cmdQuiet :: String -> CreateProcess
cmdQuiet com = CreateProcess
	{ cmdspec = ShellCommand com
	, cwd = Nothing
	, env = Nothing
	, std_in = CreatePipe
	, std_out = CreatePipe
	, std_err = Inherit
	, close_fds = True
	, create_group = False
	}
\end{code}

\ct{runCom} and \ct{cmd} are the actual workhorses that spawn the external command defined by the user.
The output of the external command is colorized using the \ct{sed} stream editor.
