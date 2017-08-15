module WeekSecond where
  
import Log
import Data.List
import Control.Monad

-- Ad 1.
parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage msg = LogMessage 
                   (parseMessageType msg) 
                   (parseMessageTimestamp msg ((parseMessageType msg)))
                   (parseMessageBody msg (parseMessageType msg)) 

parseMessageType :: String -> MessageType
parseMessageType msg = case (head . words) msg of
                    "I" -> Info
                    "W" -> Warning
                    "E" -> Error $ parseErrorMessagePriority msg
                    
parseErrorMessagePriority :: String -> Int
parseErrorMessagePriority msg = read $ (words msg) !! 1

parseMessageTimestamp :: String
                      -> MessageType
                      ->  Int 
parseMessageTimestamp msg msgType
 = read $ (words msg) !! (indexOfTimestampForMessageType msgType)

parseMessageBody :: String
                 -> MessageType
                 -> String
parseMessageBody msg msgType
 = concat $ intersperse " " $ drop (indexOfBodyForMessageType msgType) (words msg)


-- Log Message Positional Knowledge
indexOfBodyForMessageType :: MessageType -> Int
indexOfBodyForMessageType msgType = case msgType of
                           Info     -> 2
                           Warning  -> 2
                           Error _  -> 3

indexOfTimestampForMessageType :: MessageType -> Int
indexOfTimestampForMessageType msgType = case msgType of
                                Info    -> 1
                                Warning -> 1
                                Error _ -> 2
                                
-- Optional Version

-- Ad 1.
maybeParse :: String -> [LogMessage]
maybeParse = map maybeParseMessage . lines

maybeParseMessage :: String -> LogMessage
maybeParseMessage msg = case ((maybeParseMessageType msg), (maybeParseMessageTimestamp msg ((parseMessageType msg))), (maybeParseMessageBody msg (maybeParseMessageType msg))) of
                            (Just msg, Just timestamp, Just body) -> LogMessage msg timestamp body
                            (_, _, _) -> Unknown msg
  

maybeParseMessageTimestamp :: String
                           -> MessageType
                           -> Maybe Int
maybeParseMessageTimestamp msg msgType
 = read <$> maybeAtIndex (indexOfTimestampForMessageType msgType) (words msg)
 
maybeParseErrorMessagePriority :: String -> Maybe Int
maybeParseErrorMessagePriority msg = read <$> maybeAtIndex 1 (words msg)

maybeParseMessageType :: String -> Maybe MessageType
maybeParseMessageType msg = case (head . words) msg of
                    "I" -> Just Info
                    "W" -> Just Warning
                    "E" -> Just $ Error $ parseErrorMessagePriority msg
                    _   -> Nothing
                    
maybeParseMessageBody :: String
                      -> Maybe MessageType
                      -> Maybe String
maybeParseMessageBody _ (Nothing) = Nothing
maybeParseMessageBody msg (Just msgType)
 = case (concat $ intersperse " " $ drop (indexOfBodyForMessageType msgType) (words msg)) of
        ""   -> Nothing
        body -> Just body

-- Optional List Access

maybeAtIndex :: Int -> [a] -> Maybe a
maybeAtIndex _ [] = Nothing
maybeAtIndex 0 (x:_) = Just x
maybeAtIndex i (_:xs) = maybeAtIndex (i-1) xs

-- Ordering Logs

-- Ad 2.
-- Insert LogMessage into a binary tree. Binary tree is based on timestamp
logInsert :: LogMessage -> MessageTree -> MessageTree
logInsert (Unknown _) tree = tree
logInsert x Leaf = Node Leaf x Leaf
logInsert (LogMessage nmt nt nb) (Node lt (LogMessage emt et eb) rt)
          | et == nt = Node lt existing rt
          | et > nt  = Node (logInsert new lt) existing rt
          | et < nt  = Node lt existing (logInsert new rt)
          where existing = (LogMessage emt et eb)
                new = (LogMessage nmt nt nb)
                
-- Ad 3.
-- Building a tree from array of log messages
build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = logInsert x Leaf
build (x:xs) = logInsert x (build xs)

-- Ad 4.
-- Traversing tree in order
-- if we pass a sorted tree we will output a sorted array of log messages
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf x Leaf) = [x]
inOrder (Node lf x rt) = inOrder lf ++ [x] ++ inOrder rt

-- Ad 5.
-- Filtering out only Error log messages with severity > 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong [(LogMessage (Error s) _ body)] = if s > 50 then [body] else []
whatWentWrong [(LogMessage _ _ _)] = []
whatWentWrong (x:xs) = whatWentWrong [x] ++ whatWentWrong xs