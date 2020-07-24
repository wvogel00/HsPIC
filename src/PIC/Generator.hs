module PIC.Generator where

import Prelude hiding (foldl1)
import Data.Bits (complement)
import Data.Bits.Extras (w8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Codec.CBOR.Magic 
import PIC.Types

testRecord = Record {
    len = 16,
    offset = B.singleton 0,
    rectype = Data,
    _data = B.pack $ map w8 [4, 40, 255, 63, 255, 63, 255, 63, 3, 19, 131, 18, 134, 1, 131, 22],
    chksum = B.singleton 0
}

data RecType = Data
            | FileFin
            | ExSegmentAddr
            | StartSegmentAddr
            | ExLinearAddr
            | StartLinearAddr deriving (Eq, Show)

data Record = Record {
    len :: Int, -- 1byte
    offset :: B.ByteString, -- 2bytes
    rectype :: RecType, -- 1byte
    _data :: B.ByteString, -- len bytes
    chksum :: B.ByteString -- 1byte
} deriving (Eq, Show)

rectypeBits :: RecType -> B.ByteString
rectypeBits Data            = B.singleton 0
rectypeBits FileFin         = B.singleton 1
rectypeBits ExSegmentAddr   = B.singleton 2
rectypeBits StartSegmentAddr= B.singleton 3
rectypeBits ExLinearAddr    = B.singleton 4
rectypeBits StartLinearAddr = B.singleton 5

-- チェックサムを計算し付与する
insertChkSum :: Record -> B.ByteString
insertChkSum rec = B.singleton.complement2.B.foldl1 (+)
        $ (B.singleton.w8 $ len rec) `B.append` (offset rec) `B.append` (rectypeBits $ rectype rec) `B.append` (_data rec)

-- 2の補数
add w1 w2 = w8 $ mod (word8ToInt w1 + word8ToInt w2) 255
complement2 = add (w8 1).complement

-- リトルエンディアンに変換
-- littleEndian :: B.ByteString -> B.ByteString
littleEndian = B.pack.convert.B.unpack where
    convert [] = []
    convert [a] = [w8 0, a]
    convert (w1:w2:bs) = w2:w1:convert bs

-- HEXファイルの出力時に呼ぶ関数
outputHexFile :: FilePath -> [Record] -> IO ()
outputHexFile file = B.writeFile file.BC.unlines.map outputRecFormat

outputRecFormat :: Record -> B.ByteString
outputRecFormat rec = B.append (BC.pack ":") $
        (B.singleton.w8 $ len rec) `B.append` (offset rec) `B.append`
        (rectypeBits $ rectype rec) `B.append` (littleEndian $ _data rec) `B.append` (chksum rec)

-- 解析した式からHEXを生成する時に呼ぶ関数
genRecords :: [Expr] -> [Record]
genRecords = concat.map genRecord

-- 一つの式からレコード群を生成する
genRecord :: Expr -> [Record]
genRecord expr = [testRecord]

