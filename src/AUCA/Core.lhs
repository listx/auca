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

\begin{code}
data AppState = AppState
	{ timeBuffer :: TimeBuffer
	, comDef :: String
	, comSimpleFilePath :: FilePath
	, inotify :: INotify
	, opts :: Opts
	}
\end{code}

\begin{code}
data TimeBuffer = TimeBuffer
	{ bufSeconds :: NominalDiffTime
	, bufSecStockpile :: NominalDiffTime
	}
\end{code}

\subsection{Event Handling}

We only execute the given command when the detected event is a \textit{modification} event of a \ct{file}.
We ignore all other types of events, but print out info messages to tell the user what happened.
If a file becomes ignored or deleted for some reason, we re-watch it.\fn{Vim tends to delete and re-create files when saving a modification.}

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

\ct{addWD} is a simple wrapper function around the more general \ct{addWatch} function provided by \ct{System.INotify}.

\begin{code}
addWD :: INotify -> FilePath -> (Event -> IO ()) -> IO WatchDescriptor
addWD inotify fp evHandler = addWatch inotify evs fp evHandler
	where
	evs = [Attrib, Modify, DeleteSelf]
\end{code}

\subsection{Key Handling}

The keypresses are interpreted through a buffer system.
Essentially, this system works to prevent spamming the \ct{keyHandler} loop.
I.e., if a user presses and \textit{holds down} a key, without a buffering system, the loop would execute the total number of keypresses that the windowing system would allow.
Even with a modest delay between keypresses, allowing such a torrent of repeated keypresses to go through unabated would be undesirable.
Thus, \ct{keyHandler} measures the amount of time taken to process a keypress, and adds it to the buffer, called \ct{bufSecStockpile}.
If this stockpile adds up to the treshhold defined by \ct{bufSeconds}, we execute the latest keypress; otherwise, we add the amount taken by the single keypress and add it to the stockpile.

Note that if the user waits a long time, that's fine as the \ct{getChar} function will take that much longer to finish extracting the keypress.

\begin{code}
keyHandler :: StateT AppState IO ()
keyHandler = do
	appState@AppState{..} <- get
	t1 <- lift getCurrentTime
	c <- lift getChar
	when (c == 'q') . lift $ do
		killINotify inotify
		exitSuccess
	let
		tb@TimeBuffer{..} = timeBuffer
	t2 <- lift getCurrentTime
	let
		t3 = diffUTCTime t2 t1
		stockpile = t3 + bufSecStockpile
	if (stockpile >= bufSeconds)
		then do
			let
				tb' = tb {bufSecStockpile = stockpile - bufSeconds}
			put $ appState {timeBuffer = tb'}
			keyHandler' c
			keyHandler
		else do
			let
				tb' = tb {bufSecStockpile = stockpile + t3}
			put $ appState {timeBuffer = tb'}
			keyHandler
\end{code}

The \ct{comHash} and \ct{comKeys} structures define the hotkeys available to the user if multiple commands were defined.

\begin{code}
keyHandler' :: Char -> StateT AppState IO ()
keyHandler' key
	| key == 'h' = do
		AppState{..} <- get
		lift $ helpMsg opts comSimpleFilePath
	| key == 'd' = do
		appState@AppState{..} <- get
		lift $ helpMsg opts comSimpleFilePath
		lift . putStrLn $ colorize Cyan "swapping default command..."
		c <- lift getChar
		comHash <- getComHash
		case lookup [c] comHash of
			Just com -> do
				let
					opts' = opts
						{ commands = swapElems (0, toInt c)
							$ commands opts
						}
				put $ appState
					{ comDef = com
					, opts = opts'
					}
				lift $ helpMsg opts' comSimpleFilePath
			_ -> do
				lift . putStrLn . colorize Red $ unwords
					[ "key"
					, show c
					, "is not a valid command slot"
					]
	| elem key comKeys = do
		AppState{..} <- get
		comHash <- getComHash
		case lookup [key] comHash of
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
	| otherwise = do
		AppState{..} <- get
		lift $ putStrLn []
		lift showTime
		lift . putStr $ ": " ++ colorize Cyan "manual override"
		lift . putStrLn $ "; executing command "
			++ squote (colorize Blue comDef)
		lift . runCom $ cmd comDef
	where
	comKeys :: String
	comKeys = concatMap show [(0::Int)..9]
	getComHash = do
		AppState{..} <- get
		let
			coms = commands opts
			comSimple = command_simple opts
		return $ if null coms
			then [("0", comSimple ++ " " ++ comSimpleFilePath)]
			else zip (map show [(0::Int)..9]) coms
\end{code}

\ct{runCom} and \ct{cmd} are the actual workhorses that spawn the external command defined by the user.
The output of the external command is colorized using the \ct{sed} stream editor.

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
	, delegate_ctlc = True
	, env = Nothing
	, std_in = CreatePipe
	, std_out = Inherit
	, std_err = Inherit
	, close_fds = True
	, create_group = False
	}
\end{code}
