module Text.HTML.DirectoryListing.Type 
where

import qualified Data.Text as T
import Data.Time.LocalTime (LocalTime)
import Network.URI (unEscapeString)

-- | The type Entry represents a clickable link in web page.
data Entry = Entry
    { visibleName :: T.Text
    -- ^ this is a name that visible in .html
    -- (web engines will stripe name is it is too long)
    , href :: T.Text
    -- ^ href in the tag's attribute
    , lastModified :: LocalTime
    -- ^ last modified time (Note that LocalTime has Ord instance)
    , fileSize :: Maybe Integer
    -- ^ file size represented in bytes.
    -- when the Entry is a directory, this value is Nothing.
    }
    deriving Show

-- | get the real file name from an Entry (by decoding href)
decodedName :: Entry -> T.Text
decodedName = T.pack . unEscapeString . T.unpack . href

-- | is this Entry a directory?
isDirectory :: Entry -> Bool
isDirectory = (=='/') . last . T.unpack . href

