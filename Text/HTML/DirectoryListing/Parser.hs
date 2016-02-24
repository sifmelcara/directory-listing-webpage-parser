{-# LANGUAGE OverloadedStrings #-}

module Text.HTML.DirectoryListing.Parser
where

import Text.HTML.DirectoryListing.Type 
import Text.HTML.TagSoup
import Data.Time.LocalTime
import Data.Time.Format

import qualified Data.Text as T

parseFileListing :: T.Text -> [Entry]
parseFileListing html = map toEntry itemTagLines
    where
    hlines = T.lines html
    itemTagLines :: [[Tag T.Text]]
    itemTagLines = filter isItemLine . map (filter (not . isNoise) . parseTags) $ hlines
    isItemLine :: [Tag T.Text] -> Bool
    isItemLine [(TagOpen oStr [("href", _)]), TagText _, (TagClose cStr), (TagText tms)] = 
        and [ oStr == "a"
            , cStr == "a"
            , (length . T.words $ tms) == 3 -- date hhmm filesize
            ]
    isItemLine _ = False
    toEntry :: [Tag T.Text] -> Entry
    toEntry [(TagOpen _ [("href", ref)]), TagText name, (TagClose _), (TagText tms)] =
        Entry { name = name
              , href = ref
              , lastModified = parseLastModified . T.concat . take 2 . T.words $ tms
              , fileSize = parseFileSize . last . T.words $ tms
              }
    toEntry _ = error "toEntry: not an item line"

    -- | apache's directory listing have many noise, filter them out
    isNoise (TagOpen "tr" _) = True
    isNoise (TagClose "tr") = True
    isNoise (TagOpen "td" _) = True
    isNoise (TagClose "td") = True
    isNoise (TagOpen "img" _) = True
    isNoise _ = False
    
-- | Bad design, it throws error when noParse
--   some example inputs:
--     24-Apr-2014 11:55
--     04-Jan-2014 13:18
parseLastModified :: T.Text -> LocalTime
parseLastModified t = parseTimeOrError True locale format (T.unpack t) 
    where
    format = "%d-%b-%Y %R"
    locale = defaultTimeLocale
        { months = map (\y -> (y, y)) $
                    [ "Jan", "Feb", "Mar", "Apr"
                    , "May", "Jun", "Jul", "Aug"
                    , "Sep", "Oct", "Nov", "Dec"
                    ]
        }

-- | this function only accept the following format:
-- 665
-- 123B
-- 249K
-- 3.8M
-- 5.6G
-- return value: represent file size in Byte.
parseFileSize :: T.Text -> Maybe Integer
parseFileSize t = 
    case reads s::[(Double, String)] of
        [] -> Nothing
        [(f, "" )] -> Just . round $ f
        [(f, "B")] -> Just . round $ f
        [(f, "K")] -> Just . round $ f*1024
        [(f, "M")] -> Just . round $ f*1024*1024
        [(f, "G")] -> Just . round $ f*1024*1024*1024
        _ -> Nothing
    where
    s = T.unpack . T.strip $ t


