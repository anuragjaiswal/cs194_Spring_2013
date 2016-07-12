{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage str = let wordList = words str in
					case wordList of 
					("I":ts: msg) -> LogMessage Info (read ts) (unwords msg)
					("W":ts: msg) -> LogMessage Warning (read ts) (unwords msg)
					("E":lvl:ts: msg) -> LogMessage (Error (read lvl)) (read ts) (unwords msg)
					_ -> Unknown (unwords wordList)
					
parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

insert :: LogMessage -> MessageTree -> MessageTree
insert lm mt
	| mt == Leaf = Node Leaf lm Leaf
insert lm1@(LogMessage _ ts1 _) (Node left lm2@(LogMessage _ ts2 _) right)
	| ts1 > ts2 = Node left lm2 (insert lm1 right)
	| otherwise = Node (insert lm1 left) lm2 right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = (inOrder left) ++ [lm] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = map getMsg (filter (filterMsg 50) x)


filterMsg :: Int -> LogMessage -> Bool
filterMsg clvl (LogMessage (Error lvl) _ _)
	| clvl > lvl = False
	| otherwise = True
filterMsg _ _ = False
	
getMsg :: LogMessage -> String
getMsg lm@(LogMessage _ _ msg) = msg
getMsg _ = ""