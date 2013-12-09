\section{AUCA/Meta.lhs}

This module mainly defines the metadata that comes with \ct{auca}.
Of particular notice here is the version number definition.

\begin{code}
module AUCA.Meta where

_PROGRAM_NAME
	, _PROGRAM_VERSION
	, _PROGRAM_INFO
	, _PROGRAM_DESC
	, _COPYRIGHT :: String
_PROGRAM_NAME = "auca"
_PROGRAM_VERSION = "0.0.2"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_DESC = "execute arbitrary command(s) based on file changes"
_COPYRIGHT = "(C) Linus Arver 2011"
\end{code}
