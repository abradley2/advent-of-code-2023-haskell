{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Relude

import Data.ByteString qualified as BS
import Data.List qualified
import Data.Text qualified as Text

wordDigits :: [(Text, Text)]
wordDigits =
  second (show :: Int -> Text)
    <$> [ ("one", 1)
        , ("two", 2)
        , ("three", 3)
        , ("four", 4)
        , ("five", 5)
        , ("six", 6)
        , ("seven", 7)
        , ("eight", 8)
        , ("nine", 9)
        ]

replaceWordDigits :: ByteString -> ByteString
replaceWordDigits bs = encodeUtf8 $ Data.List.foldl reduce (decodeUtf8 bs) wordDigits
 where
  reduce text (search, replace) =
    mconcat $ Data.List.intersperse (search <> replace <> search) $ Text.splitOn search text

digits :: ByteString
digits = "0123456789"

digitVal :: Word8 -> Maybe Int
digitVal w = BS.elemIndex w digits

firstAndLastDigit :: ByteString -> Either ByteString (Int, Int)
firstAndLastDigit bs = loop (Nothing, Nothing) $ BS.unpack bs
 where
  loop (f, l) (w : next) = loop (f <|> digitVal w, digitVal w <|> l) next
  loop (Just f, Just l) [] = Right (f, l)
  loop _ _ = Left $ "Invalid input? " <> bs

partOne :: [ByteString] -> ByteString
partOne parsedInput =
  case mapM firstAndLastDigit parsedInput of
    Left err -> err
    Right pairs -> show . getSum $ foldMap ((Sum <$> uncurry (+)) . first (* 10)) pairs

partTwo :: [ByteString] -> ByteString
partTwo parsedInput =
  case mapM (firstAndLastDigit . replaceWordDigits) parsedInput of
    Left err -> err
    Right pairs -> show . getSum $ foldMap ((Sum <$> uncurry (+)) . first (* 10)) pairs

main :: IO ()
main = do
  parsedInput <- BS.split (BS.head "\n") <$> readFileBS "day-01/input.txt"
  putBSLn $ "Part One: " <> partOne parsedInput
  putBSLn $ "Part Two: " <> partTwo parsedInput
