module Type
where

import qualified Data.Text as T

data Entry = Entry
    { name :: T.Text
    , href :: T.Text
    , lastModified :: T.Text
    , fileSize :: Maybe T.Text
    }

