{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

basicLog1,basicLog2,basicLog3 :: LogMessage
basicLog1  = LogMessage Info 1 "1"
basicLog2 = LogMessage Info 2 "2"
basicLog3 = LogMessage Info 3 "3"

basicLog4 :: [LogMessage]
basicLog4 = [LogMessage Info 1 "1",LogMessage Info 2 "2",LogMessage Info 3 "3"]

-- Gets info LogMessage from a line of text
getInfoMessage :: [String] -> LogMessage
getInfoMessage [] = Unknown ""
getInfoMessage [x] = Unknown x
getInfoMessage (time:message) = LogMessage Info (read time :: TimeStamp) (unwords message)

-- Gets warning LogMessage from a line of text
getWarningMessage :: [String] -> LogMessage
getWarningMessage [] = Unknown ""
getWarningMessage [x] = Unknown x
getWarningMessage (time:message) = LogMessage Warning (read time :: TimeStamp) (unwords message)

-- Gets error LogMessage from a line of text
getErrorMessage :: [String] -> LogMessage
getErrorMessage [] = Unknown ""
getErrorMessage [x] = Unknown x
getErrorMessage (err:time:message) = LogMessage (Error (read err :: Int)) (read time :: TimeStamp) (unwords message)

-- Parses a line of text to output a LogMessage
parseMessage :: String -> LogMessage
parseMessage [] = Unknown ""
parseMessage (x:xs) = case x of
                        'I' -> getInfoMessage (words xs)
                        'W' -> getWarningMessage (words xs)
                        'E' -> getErrorMessage (words xs)
                        _ -> Unknown (x:xs)

-- A more concise way to parse seen online
parseMessage2 :: String -> LogMessage --added after I looked up examples and saw good syntax
parseMessage2 str = let allWords = words str in
                   case allWords of
                        ("I":time:msg) -> LogMessage Info (read time :: TimeStamp) (unwords msg)
                        ("W":time:msg) -> LogMessage Warning (read time :: TimeStamp) (unwords msg)
                        ("E":err:time:msg) -> LogMessage (Error (read err :: Int)) (read time :: TimeStamp) (unwords msg)
                        _ -> Unknown (unwords allWords)

-- Parse a file and output a list of LogMessages
parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

-- Insert a LogMessage into a sorted binary search tree
insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ time _) (Node leftRoot rootMsg@(LogMessage _ rootTime _) rightRoot) 
        | time < rootTime = Node (insert msg leftRoot) rootMsg rightRoot
        | time > rootTime = Node leftRoot rootMsg (insert msg rightRoot)
insert _ input = input -- removes unknown messages from the tree

-- Builds a sorted binary search tree from a collection of LogMessages
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)
-- build xs = foldr insert Leaf xs --hlint suggestion, leaving since foldr is not introduced

-- produces a list of sorted LogMessages by timestamp from a binary search
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) =  inOrder left ++ [msg] ++ inOrder right -- Haskell is crazy for this

-- Lists errors with severity >= a severity level
listSevereErrors :: [LogMessage] -> Int -> [String]
listSevereErrors [] _ = []
listSevereErrors msgs sevLvl = [message | (LogMessage (Error sev) _ message) <- msgs, sev >= sevLvl]

-- Shows errors with severity > 50. Hardcoded to match assignment
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong x = listSevereErrors (inOrder (build x)) 50
