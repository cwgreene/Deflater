import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import qualified Data.ByteString.UTF8 as UTF8
import Data.Bits
import Numeric
import qualified Data.ByteString.Char8 as BC

data Hex8 = Hex8 Word8
data FileName = None | FileName String deriving Show

instance Show Hex8 where
    show (Hex8 x) = "0x" ++ (showHex x "")

data GzipFile = GzipFile {
        id1 :: Hex8,
        id2 :: Hex8,
        cm :: Word8,
        flg :: Word8,
        mtime :: Word32,
        xfl :: Word8,
        os :: Word8,
        fname :: FileName
    } deriving Show

readUntilZero :: [Word8] -> Get FileName
readUntilZero acc = do
    nextByte <- getWord8
    if nextByte == 0 then
        return $ FileName $ BC.unpack $ B.pack acc
    else
        readUntilZero (acc ++ [nextByte])

getFileName flg =
    if (flg .&. 8) /= 0 then
        readUntilZero []
    else
        return None

parseGzip = do
    id1 <- getWord8
    id2 <- getWord8
    cm <- getWord8
    flg <- getWord8
    mtime <- getWord32be
    xfl <- getWord8
    os <- getWord8
    fname <- getFileName flg
    return (GzipFile (Hex8 id1) (Hex8 id2) cm flg mtime xfl os fname)
{-    xbytes <- readXBytes flg
    fextra <- readFExtra flg
    fname <- readFName flg
    fcomment <- readFComment flg
    crc16 <- readFHCRC flg
    remaining <- getByteString
    -}

main = do
    input <- BL.getContents
    putStrLn $ show $ runGet parseGzip input