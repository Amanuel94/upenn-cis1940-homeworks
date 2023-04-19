{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module LogAnalysis where
import Log
import Control.Arrow (ArrowChoice(right))
import Data.Tree (Tree(rootLabel))
------------------------------------------------

--Exercise 1
parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
    ("I":timestamp:str) -> LogMessage Info (read timestamp) (unwords str)
    ("W":timestamp:str) ->LogMessage Warning (read timestamp) (unwords str)
    ("E":lvl:timestamp:str) ->LogMessage (Error (read lvl)) (read timestamp) (unwords str)
    _ -> Unknown msg

parse :: String -> [LogMessage]
parse msgs = map parseMessage (lines(msgs))

------------------------------------------------

--Exercise 2

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp( LogMessage _ timeStamp _ ) =  timeStamp

getChild :: MessageTree -> String -> MessageTree
getChild (Node left _ _ ) "left" = left
getChild (Node _ _ right ) "right" = right

getMsg :: MessageTree -> LogMessage
getMsg (Node _ msg _) = msg

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) root = root
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node leftTree nodeMsg rightTree)
    | getTimeStamp msg < getTimeStamp nodeMsg = Node (insert msg leftTree) nodeMsg rightTree
    | otherwise = Node leftTree nodeMsg (insert msg rightTree)

------------------------------------------------

--Exercise 3

buildTree :: [LogMessage] -> Int -> MessageTree -> MessageTree
buildTree msgs 0 root = root
buildTree msgs n root = buildTree msgs (n-1) (insert (msgs!!(n-1)) root)

build :: [LogMessage] -> MessageTree
build msgs = buildTree msgs (length msgs) Leaf







main = do
     contents <- testParse parse 10 "error.log"
     print (contents!!7)

