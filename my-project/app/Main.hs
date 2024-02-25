{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

import Lib
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value(..), object, (.=), toJSON, ToJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (toJSONKey)
import Data.Maybe (mapMaybe, Maybe(..), isNothing)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class
import qualified Data.Text as Text
import Text.Read
import Data.Typeable
import Control.Exception (throwIO, IOException, Exception)
import Data.Char (toLower)


data MyValue = MyDate (Int, Int, Int) | MyNumber Integer | MyString Text | MyBool Bool | MyArray [Text] | MyBoolArray[Bool] deriving Show

data ParsingException = ParsingException String
    deriving (Show, Exception)


instance ToJSON MyValue where
    toJSON (MyDate (y, m, d)) = object ["year" .= y, "month" .= m, "day" .= d]
    toJSON (MyNumber n) = toJSON n
    toJSON (MyString s) = toJSON s
    toJSON (MyBool b) = toJSON b
    toJSON (MyArray a) = toJSON a
    toJSON (MyBoolArray e) = toJSON e

parseComponent :: String -> String -> IO (Maybe Int)
parseComponent componentLabel componentValue = do
    let maybeParsed = readMaybe componentValue :: Maybe Int
    case maybeParsed of
        Just parsed -> return (Just parsed)
        Nothing     -> do
            putStrLn $ "Failed to parse " ++ componentLabel ++ ": " ++ componentValue
            return Nothing

isText :: Typeable a => a -> Bool
isText x = typeOf x == typeOf ("" :: Text)

isBool :: Text -> Bool
isBool value = value `elem` ["Y", "T", "t", "y", "N", "F", "f"]

allBools :: [Text] -> Bool
allBools = all isBool

convertToBoolList :: [Text] -> [Bool]
convertToBoolList xs = [isBool x | x <- xs]

parseEntity :: Text -> IO (Maybe (Text, MyValue))
parseEntity pair = case T.splitOn "|" pair of
    [key, value] -> case T.unpack $ T.take 2 key of
        "00" -> do
            maybeDate <- parseDate value
            case maybeDate of
                Just d -> return $ Just (Text.drop 2 key, MyDate d)
                Nothing -> return Nothing
        "01" -> do
            let res = readMaybe (T.unpack value) :: Maybe Int
            case res of
                Just v -> return $ Just (Text.drop 2 key, MyNumber $ fromIntegral v)
                Nothing -> throwIO (ParsingException ("PARSING_FAILED_IN_CASE_01: " <> show (value)))
        "02" -> return $ Just (Text.drop 2 key, MyString value)
        "03" -> do
            if(isBool value) then do
                if(value `elem` ["t", "T", "y", "Y"]) then return $ Just (Text.drop 2 key, MyBool $ True)
                    else return $ Just (Text.drop 2 key, MyBool $ False)
            else throwIO (ParsingException ("PARSING_FAILED_IN_CASE_03_CHAR_NOT_REPRESENT_IN_BOOL_FORMAT: " <> show (value)))
        "10" -> do
            let val = T.splitOn "," value
                firstEle = head val
                res = readMaybe (T.unpack firstEle) :: Maybe Int
            case res of 
                Just v -> return $ Just (Text.drop 2 key, MyArray $ val)
                Nothing -> throwIO (ParsingException ("PARSING_FAILED_IN_CASE_10: " <> show (value)))
            return $ Just (Text.drop 2 key, MyArray $ T.splitOn "," value)
        "11" -> do
            let val = T.splitOn "," value
                firstEle = head val
                res = readMaybe (T.unpack firstEle) :: Maybe Int
            case res of 
                Just v -> return $ Just (Text.drop 2 key, MyArray $ val)
                Nothing -> throwIO (ParsingException ("PARSING_FAILED_IN_CASE_11: " <> show (value)))
        "12" -> do
            let val = T.splitOn "," value
                firstEle = head val
            if all isText val then
                return $ Just (Text.drop 2 key, MyArray $ val)
                else throwIO (ParsingException ("PARSING_FAILED_IN_CASE_12: " <> show (value)))
        "13" -> do
            let val = T.splitOn "," value
            if (allBools val) then return $ Just (Text.drop 2 key, MyBoolArray $ convertToBoolList val)
            else throwIO (ParsingException ("PARSING_FAILED_IN_CASE_13_NOT_PRESENT_IN_BOOL_FORMAT: " <> show (value)))
        _     -> do
            throwIO (ParsingException ("WRONG_FORMAT_CODES_FOR_DATA: " <> show (value)))
    _            -> do
        return Nothing
    where 
        parseDate s = case map T.unpack (T.splitOn "-" s) of
            [y, m, d] -> do
                maybeYear <- parseComponent "year" y
                maybeMonth <- parseComponent "month" m
                maybeDay <- parseComponent "day" d
                case (maybeYear, maybeMonth, maybeDay) of
                    (Just year, Just month, Just day) -> return $ Just (year, month, day)
                    _ -> throwIO (ParsingException "Failed to parse date")


resultToJson :: Maybe (Text, MyValue) -> IO (Maybe Value)
resultToJson Nothing          = return Nothing
resultToJson (Just (t, val)) = return $ Just $ object ["key" .= t, "value" .= val]


parseToJSON :: Text -> IO (Maybe [Text])
parseToJSON input = do
    let pairs = T.splitOn "#" input
    let nonEmptyPairse = filter (/= "") pairs
    entities <- traverse parseEntity nonEmptyPairse
    let filteredEntities = mapMaybe id entities
    let jsonPairs = map (\(k, v) -> (TE.encodeUtf8 k, toJSON v)) filteredEntities
    val <- traverse resultToJson entities
    print val 
    return (Just pairs)


-- Example input string
input :: Text
input = "#00|1997-02-06#02name|bob#01age|20#03hasPassport|f#12access|read_db,write_db,view_logs"

main :: IO ()
main = do
    result <- parseToJSON input
    case result of
        Just json -> print "Successfully decoded"
        Nothing   -> putStrLn "Failed to parse input string"



-- test cases

-- "#00date|1997-02-06#02name|bob#01age|20#03hasPassport|f#12access|read_db,write_db,view_logs" --Happy case

-- Case 1 : "#00date|199a7-02-06#02name|bob#01age|20#03hasPassport|Y#11access|1,2,3" -> We are passing wrong date 
-- Case 2 : "#00date|1997-02-06#02name|bob#01age|20#03hasPassport|Y#11access|a,b,c" -> We are passing wrong wrong types of data in array
-- Case 3 : "#00date|1997-02-06#02name|bob#01age|20#03hasPassport|z#12access|read_db,write_db,view_logs" -- we are passing wrong boolean char
-- Case 4 : "#0date|1997-02-06#02name|bob#01age|20#03hasPassport|f#12access|read_db,write_db,view_logs"

-- Result 1 : Failed to parse year: 199a7 *** Exception: ParsingException "Failed to parse date"
-- Result 2 : *** Exception: ParsingException "PARSING_FAILED_IN_CASE_11: \"a,b,c\""
-- Result 3 : *** Exception: ParsingException "PARSING_FAILED_IN_CASE_03_CHAR_NOT_REPRESENT_IN_BOOL_FORMAT: \"z\""
-- Result 4 : *** Exception: ParsingException "WRONG_FORMAT_CODES_FOR_DATA: \"1997-02-06\""

-- All possible Errors that we can handle

-- Case1 : if key is empty string
-- Case2 : if value is empty
-- Case3 : if format is incorrect like ||, ## found or anything
-- Case 4 : if key or value is null