{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module LogAnalysis where
import Log
import GHC.Exts.Heap.Closures (GenClosure(FloatClosure))


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

------------------------------------------------

--Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) =
    let left_part = inOrder left
        whole_part = left_part ++ [msg]
    in whole_part ++ inOrder right

------------------------------------------------

--Exercise 5

filt :: LogMessage -> Bool
filt (LogMessage (Error strength) _ _) = strength >= 50
filt _ = False

filterByErrorStrength :: [LogMessage] -> [LogMessage]
filterByErrorStrength = filter (filt)



whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMsgs = 
    let sortedMsgs = inOrder (build logMsgs)
    in map (\(LogMessage _ _ msg) -> msg) (filterByErrorStrength sortedMsgs)
whatWentWrong _ = []