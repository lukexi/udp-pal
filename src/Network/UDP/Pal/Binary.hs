module Network.UDP.Pal.Binary where
import           Data.Binary
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as L

-- | Encode a value to a strict bytestring
encode' :: Binary a => a -> ByteString
encode' = L.toStrict . encode

-- | Decode a value from a strict bytestring
decode' :: Binary c => ByteString -> c
decode' = decode . L.fromStrict
