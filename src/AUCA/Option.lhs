\section{AUCA/Option.lhs}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module AUCA.Option where

import System.Console.CmdArgs.Implicit

import AUCA.Meta
import AUCA.Util

data Opts = Opts
	{ command :: [String]
	, command_simple :: String
	, file :: [FilePath]
	, list :: FilePath
	, interval :: Int
	} deriving (Data, Typeable, Show, Eq)

progOpts :: Opts
progOpts = Opts
	{ command = def &= typ "COMMAND" &= help "command(s) to execute; up to 10 (hotkeyed to 1-0)"
	, command_simple = def &= typ "COMMAND" &= name "C" &= help "command to execute; it takes the first file, and calls command after it; e.g., `-C lilypond -f foo.ly' will translate to `lilypond foo.ly' as the default command"
	, file = def &= help "file(s) to watch; can be repeated multiple times to define multiple files"
	, list = def &= help "list of files to watch"
	, interval = 0 &= typ "SECONDS" &= help "sleep SECONDS amount of time and detect changes only on these intervals"
	}
	&= details
		[ "Notes:"
		, ""
		, "  All commands are passed to the default shell."
		]

getOpts :: IO Opts
getOpts = cmdArgs $ progOpts
	&= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
	&= program _PROGRAM_NAME
	&= help _PROGRAM_DESC
	&= helpArg [explicit, name "help", name "h"]
	&= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]

helpMsg :: Opts -> FilePath -> IO ()
helpMsg Opts{..} f = do
	mapM_ showCom $ if null command
		then [("1", command_simple ++ " " ++ f)]
		else zip (map show [(1::Int)..10]) command
	putStrLn "press `h' for help"
	putStrLn "press `q' to quit"
	putStrLn $ "press any other key to execute the default command " ++
		squote (colorize Blue comDef)
	where
	showCom :: (String, String) -> IO ()
	showCom (a, b) = putStrLn $ "key "
		++ squote (colorize Yellow a)
		++ " set to "
		++ squote (colorize Blue b)
	comDef = if null command
		then command_simple ++ " " ++ f
		else head command
\end{code}
