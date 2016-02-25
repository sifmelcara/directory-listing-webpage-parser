{-# LANGUAGE OverloadedStrings #-}

module Text.HTML.DirectoryListing.Parser
where

import Text.HTML.DirectoryListing.Type 
import Text.HTML.TagSoup
import Data.Time.LocalTime
import Data.Time.Format
import Data.List
import Data.Maybe
import Debug.Trace

import qualified Data.Text as T

parseFileListing :: T.Text -> [Entry]
parseFileListing html = catMaybes fileLines
    where
    fileLines :: [Maybe Entry]
    fileLines = map (toEntry {-. traceShowId-} . filter (not . isNoise) . parseTags) . T.lines $ html
    toEntry :: [Tag T.Text] -> Maybe Entry
    toEntry [(TagOpen "a" [("href", ref)]), TagText name, (TagClose "a"), (TagText dateTimeAndFilesize)] 
        | (length . T.words $ dateTimeAndFilesize) /= 3 = Nothing
        | otherwise = toEntry [(TagOpen "a" [("href", ref)]), TagText name, (TagClose "a"), (TagText dateTime), (TagText filesize)]
        where
        dateTime = T.concat . intersperse " " . take 2 . T.words $ dateTimeAndFilesize
        filesize = last . T.words $ dateTimeAndFilesize
    toEntry [(TagOpen "a" [("href", ref)]), TagText name, (TagClose "a"), (TagText dateTime), (TagText filesize)] = 
        Just $ Entry { visibleName = name
                     , href = ref
                     , lastModified = parseLastModified dateTime
                     , fileSize = parseFileSize filesize
                     }
    toEntry _ = Nothing

    -- | apache's directory listing have many noise, filter them out
    isNoise (TagOpen "tr" _) = True
    isNoise (TagClose "tr") = True
    isNoise (TagOpen "td" _) = True
    isNoise (TagClose "td") = True
    isNoise (TagOpen "img" _) = True
    isNoise (TagText t) = t' `elem` ["&nbsp;", "\160", ""]
        where
        t' = T.strip t
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


