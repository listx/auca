\section{AUCA/Option.lhs}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module AUCA.Option where

import System.Console.CmdArgs.Implicit

import AUCA.Meta

data Opts = Opts
	{ commands :: [String]
	, command_simple :: String
	, file :: [FilePath]
	, list :: FilePath
	, buffer_seconds :: Int
	} deriving (Data, Typeable, Show, Eq)
\end{code}

\ct{progOpts} is the data structure that actually defines all options and also describes their help messages.

\begin{code}
progOpts :: Opts
progOpts = Opts
	{ commands = def &= typ "COMMAND(S)"
		&= help "command(s) to execute; up to 10 (hotkeyed to 1-0)"
	, command_simple = def &= typ "COMMAND" &= name "C"
		&= help (unwords
			[ "command to execute; it takes the first file, and calls command"
			, "after it; e.g., `-C lilypond -f foo.ly' will translate to"
			, "`lilypond foo.ly' as the default command"
			])
	, file = def
		&= help (unwords
			[ "file(s) to watch; can be repeated multiple times to define"
			, "multiple files"
			])
	, list = def
		&= help "list of files to watch"
	, buffer_seconds = 2
		&= help "minimum interval of seconds to process file changes/keystrokes"
	}
	&= details
		[ "Notes:"
		, ""
		, "  All commands are passed to the default shell."
		]
\end{code}

\ct{getOpts} is the custom IO action that gets the options from the environment.
It also explicitly sets the `\ct{-h}' and `\ct{-v}' flags, to override the ones given by \ct{CmdArgs} (which define `\ct{-?}' as \ct{--help} and `\ct{-v}' as `\ct{--verbose}').

\begin{code}
getOpts :: IO Opts
getOpts = cmdArgs $ progOpts
	&= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
	&= program _PROGRAM_NAME
	&= help _PROGRAM_DESC
	&= helpArg [explicit, name "help", name "h"]
	&= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
\end{code}
