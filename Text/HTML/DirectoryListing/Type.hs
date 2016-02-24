module Text.HTML.DirectoryListing.Type 
where

import qualified Data.Text as T
import Data.Time.Format
import Data.Time.LocalTime (LocalTime)

data Entry = Entry
    { name :: T.Text
    , href :: T.Text
    , lastModified :: LocalTime
    , fileSize :: Maybe Integer
    -- ^ file size represented in bytes,
    -- Nothing if this Entry is a folder
    }
    deriving Show




