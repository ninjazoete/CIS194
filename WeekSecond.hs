module WeekSecond where
  
import Log
import Data.List

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
 = read $ (words msg) !!(indexOfTimestampForMessageType msgType)

parseMessageBody :: String
                 -> MessageType
                 -> String
parseMessageBody msg msgType
 = concat $ intersperse " " $ drop (indexOfBodyForMessageType msgType) (words msg)


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
                                
