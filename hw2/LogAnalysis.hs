{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = classify (words s)
  where classify ("I":ts:xs) = LogMessage Info (read ts) (unwords xs)
        classify ("W":ts:xs) = LogMessage Warning (read ts) (unwords xs)
        classify ("E":n:ts:xs) = LogMessage (Error (read n)) (read ts) (unwords xs)
        classify l = Unknown (unwords l)

parse :: String -> [LogMessage]
parse file = parseLines (lines file)
  where parseLines [] = []
        parseLines (x:xs) = parseMessage x : parseLines xs


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) node = node
insert lm@(LogMessage _ _ _) node
  | node == Leaf = (Node Leaf lm Leaf)
  | otherwise = classify lm node node
  where classify logm child parent
          | child == Leaf && isLower logm parent = (Node Leaf logm parent)
          | child == Leaf && not(isLower logm parent) = (Node parent logm Leaf)
          | isLower logm child = classify logm (getLeft child) child
          | otherwise = classify logm (getRight child) child
          where isLower logmessage comparedNode
                  | (getLogTS logmessage) <= (getNodeTS comparedNode) = True
                  | otherwise = False
                getLogTS (LogMessage _ ts _) = ts
                getNodeTS (Node _ logmessage _) = getLogTS logmessage
                getLeft (Node left _ _) = left
                getRight (Node _ _ right) = right
