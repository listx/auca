\section{AUCA/Core.lhs}

There are two main functions here --- \ct{eventHandler} and \ct{keyHandler}.
\ct{eventHandler} hooks into the \ct{inotify} API for executing arbitrary commands, and \ct{keyHandler} handles all interactive key presses by the user.

\begin{code}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

module AUCA.Core where

import Control.Monad
import "monads-tf" Control.Monad.State
import Data.Time.Clock
import System.Exit
import System.INotify
import System.Process

import AUCA.Option
import AUCA.Util
\end{code}

\subsection{Event Handling}

\begin{code}
eventHandler :: String -> FilePath -> INotify -> Event -> IO ()
eventHandler comDef fp inotify ev = case ev of
	Attributes{..} -> runCom'
	Modified{..} -> runCom'
	Ignored -> runCom'
	DeletedSelf -> do
		_ <- addWD inotify fp (eventHandler comDef fp inotify)
		return ()
	_ -> showInfo
	where
	showInfo = putStrLn ("File: " ++ fp ++ " Event: " ++ show ev)
	runCom' = do
		putStrLn []
		showTime
		putStr $ ": " ++ colorize Magenta "change detected on file " ++ squote fp
		putStrLn $ "; executing command " ++ squote (colorize Blue comDef)
		runCom $ cmd comDef
\end{code}

We only execute the given command when the detected event is a \textit{modification} event of a \ct{file}.
We ignore all other types of events, but print out info messages to tell the user what happened.
If a file becomes ignored or deleted for some reason, we re-watch it.\fn{Vim tends to delete and re-create files when saving a modification.}

\begin{code}
addWD :: INotify -> FilePath -> (Event -> IO ()) -> IO WatchDescriptor
addWD inotify fp evHandler = addWatch inotify evs fp evHandler
	where
	evs = [Attrib, Modify, DeleteSelf]
\end{code}

\ct{addWD} is a simple wrapper function around the more general \ct{addWatch} function provided by \ct{System.INotify}.

\subsection{Key Handling}

\begin{code}
data TimeBuffer = TimeBuffer
	{ bufSeconds :: NominalDiffTime
	, bufSecStockpile :: NominalDiffTime
	}
\end{code}

The keypresses are interpreted through a buffer system.
Essentially, this system works to prevent spamming the \ct{keyHandler} loop.
I.e., if a user presses and \textit{holds down} a key, without a buffering system, the loop would execute the total number of keypresses that the windowing system would allow.
Even with a modest delay between keypresses, allowing such a torrent of repeated keypresses to go through unabated would be undesirable.
Thus, \ct{keyHandler} measures the amount of time taken to process a keypress, and adds it to the buffer, called \ct{bufSecStockpile}.
If this stockpile adds up to the treshhold defined by \ct{bufSeconds}, we execute the latest keypress; otherwise, we add the amount taken by the single keypress and add it to the stockpile.

Note that if the user waits a long time, that's fine as the \ct{getChar} function will take that much longer to finish extracting the keypress.

\begin{code}
keyHandler :: Opts -> String -> FilePath -> INotify -> StateT TimeBuffer IO ()
keyHandler o@Opts{..} comDef f inotify = do
	t1 <- lift getCurrentTime
	c <- lift getChar
	when (c == 'q') . lift $ do
		killINotify inotify
		exitSuccess
	tb@TimeBuffer{..} <- get
	t2 <- lift getCurrentTime
	let
		t3 = diffUTCTime t2 t1
		stockpile = t3 + bufSecStockpile
	if (stockpile >= bufSeconds)
		then do
			put $ tb { bufSecStockpile = stockpile - bufSeconds }
			keyHandler' c
		else do
			put $ tb { bufSecStockpile = stockpile + t3 }
			keyHandler o comDef f inotify
	where
	keyHandler' 'h' = do
		lift $ helpMsg o f
		keyHandler o comDef f inotify
	keyHandler' 'q' = do
		lift $ putStrLn []
		lift $ killINotify inotify
	keyHandler' key = do
		if elem key comKeys
			then case lookup [key] comHash of
				Just com -> do
					lift $ putStrLn []
					lift $ showTime
					lift . putStr $ ": "
						++ colorize Cyan "manual override"
						++ " (slot "
						++ colorize Yellow [key]
						++ ")"
					lift . putStrLn $ "; executing command "
						++ squote (colorize Blue com)
					lift . runCom $ cmd com
				_ -> do
					lift $ putStrLn []
					lift . putStrLn $ "command slot for key "
						++ squote (colorize Yellow [key]) ++ " is empty"
			else do
				lift $ putStrLn []
				lift showTime
				lift . putStr $ ": " ++ colorize Cyan "manual override"
				lift . putStrLn $ "; executing command "
					++ squote (colorize Blue comDef)
				lift . runCom $ cmd comDef
		keyHandler o comDef f inotify
	comHash :: [(String, String)]
	comHash = if null command
		then [("1", command_simple ++ " " ++ f)]
		else zip (map show [(1::Int)..10]) command
	comKeys :: String
	comKeys = concatMap show [(0::Int)..9]
\end{code}

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
\end{code}

\ct{runCom} and \ct{cmd} are the actual workhorses that spawn the external command defined by the user.
The output of the external command is colorized using the \ct{sed} stream editor.
