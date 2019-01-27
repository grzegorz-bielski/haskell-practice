import qualified Data.ByteString.Lazy as L 
import qualified Data.ByteString as S  

bs = L.pack [99, 97, 110]
nonBs = L.unpack bs