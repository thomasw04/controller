
module Types (Error (..)) where

import Data.Text (Text)

data Error = Timeout | Failure Text
