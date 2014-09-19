\section{AUCA/Util.lhs}

\begin{code}
module AUCA.Util where

import Control.Concurrent.STM
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
\end{code}

Concurrency variable handling.

\begin{code}
-- Some mutable variable primitives; adapted from wxHaskell.
type Var a = TVar a

-- | Create a fresh mutable variable.
varCreate :: a -> IO (Var a)
varCreate = newTVarIO

-- | Get the value of a mutable variable.
varGet :: Var a -> IO a
varGet v = atomically $ readTVar v

-- | Set the value of a mutable variable.
varSet :: Var a -> a -> IO ()
varSet v x = atomically $ writeTVar v x

-- | Swap the value of a mutable variable.
varSwap :: Var a -> a -> IO a
varSwap v x = atomically $ do
	prev <- readTVar v
	writeTVar v x
	return prev

-- | Update the value of a mutable variable and return the old value.
varUpdate :: Var a -> (a -> a) -> IO a
varUpdate v f = atomically $ do
	x <- readTVar v
	writeTVar v (f x)
	return x
\end{code}

\ct{colorize} adds special ANSI escape sequences to colorize text for output in a terminal.

\begin{code}
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

\ct{errMsg} and \ct{errMsgNum} are helper functions to ease reporting simple errors.

\begin{code}
errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg

errMsgNum :: String -> Int -> IO Int
errMsgNum str num = errMsg str >> return num
\end{code}

\ct{squote} quotes a string with single quotes.
\ct{showTime} displays the current local zoned time.

\begin{code}
squote :: String -> String
squote s = "`" ++ s ++ "'"

showTime :: IO ()
showTime = getZonedTime >>= putStr . show
\end{code}

\ct{swapElems} swaps two elements in a list.
It does nothing if any of the arguments are invalid.

\begin{code}
swapElems :: (Int, Int) -> [a] -> [a]
swapElems (a, b) xs
	| null xs = xs
	| length xs == 1 = xs
	| a < 0 = xs
	| b < 0 = xs
	| a == b = xs
	| a > (length xs - 1) = xs
	| b > (length xs - 1) = xs
	| b < a = swapElems (b, a) xs
	| otherwise = preA
		++ [xs!!b]
		++ betweenAB
		++ [xs!!a]
		++ postB
	where
	preA = take a xs
	betweenAB = drop (a + 1) $ take b xs
	postB = drop (b + 1) xs
\end{code}

\begin{code}
toInt :: Char -> Int
toInt c = case c of
	'0' -> 0
	'1' -> 1
	'2' -> 2
	'3' -> 3
	'4' -> 4
	'5' -> 5
	'6' -> 6
	'7' -> 7
	'8' -> 8
	'9' -> 9
	_ -> 0
\end{code}
