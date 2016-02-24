module Text.HTML.DirectoryListing.Utils
where

import Network.URI (unEscapeString)
import Text.HTML.DirectoryListing.Type
import Data.Ord
import qualified Data.Text as T

decodedName :: Entry -> T.Text
decodedName = T.pack . unEscapeString . T.unpack . href

compareDate :: Entry -> Entry -> Ordering
compareDate = comparing lastModified

compareName :: Entry -> Entry -> Ordering
compareName = comparing decodedName

