module Crypt.Hash.SHA256
  ( sha256
  ) where

import Data.ByteString (ByteString)

foreign import sha256 :: ByteString -> ByteString
