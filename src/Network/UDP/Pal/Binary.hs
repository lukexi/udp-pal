module Network.UDP.Pal.Binary where
import Data.Binary
import qualified Data.ByteString.Lazy      as L
import Data.ByteString (ByteString)

-- | Encode a value to a strict bytestring
encode' :: Binary a => a -> ByteString
encode' = L.toStrict . encode

-- | Decode a value from a strict bytestring
decode' :: Binary c => ByteString -> c
decode' = decode . L.fromStrict