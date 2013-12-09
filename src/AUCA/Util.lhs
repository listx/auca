\section{AUCA/Util.lhs}

\begin{code}
module AUCA.Util where

import Data.Time.LocalTime
import System.IO

data Color
	= Red
	| Green
	| Yellow
	| Blue
	| Magenta
	| Cyan
	deriving (Show, Eq)

colorize :: Color -> String -> String
colorize c s = c' ++ s ++ e
	where
	c' = "\x1b[" ++ case c of
		Red -> "1;31m"
		Green -> "1;32m"
		Yellow -> "1;33m"
		Blue -> "1;34m"
		Magenta -> "1;35m"
		Cyan -> "1;36m"
	e = "\x1b[0m"
\end{code}

\ct{colorize} adds special ANSI escape sequences to colorize text for output in a terminal.

\begin{code}
errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg

errMsgNum :: String -> Int -> IO Int
errMsgNum str num = errMsg str >> return num
\end{code}

\ct{errMsg} and \ct{errMsgNum} are helper functions to ease reporting simple errors.

\begin{code}
squote :: String -> String
squote s = "`" ++ s ++ "'"

showTime :: IO ()
showTime = getZonedTime >>= putStr . show
\end{code}

\ct{squote} quotes a string with single quotes.
\ct{showTime} displays the current local zoned time.
