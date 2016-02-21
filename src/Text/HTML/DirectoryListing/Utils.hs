module Utils
where

import Network.URI (unEscapeString)
import Type
import qualified Data.Text as T

decodedName :: Entry -> T.Text
decodedName = T.pack . unEscapeString . T.unpack . href

compareDate :: T.Text -> T.Text -> Ordering
compareDate _ _ = EQ

