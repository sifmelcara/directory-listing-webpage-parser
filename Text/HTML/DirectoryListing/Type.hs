module Text.HTML.DirectoryListing.Type 
where

import qualified Data.Text as T
import Data.Time.LocalTime (LocalTime)
import Network.URI (unEscapeString)

data Entry = Entry
    { visibleName :: T.Text
    , href :: T.Text
    , lastModified :: LocalTime
    , fileSize :: Maybe Integer
    -- ^ file size represented in bytes,
    -- this value is Nothing if this Entry is a folder
    }
    deriving Show

decodedName :: Entry -> T.Text
decodedName = T.pack . unEscapeString . T.unpack . href

