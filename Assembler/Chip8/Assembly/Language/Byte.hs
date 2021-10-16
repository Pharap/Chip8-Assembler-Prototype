module Chip8.Assembly.Language.Byte where

    import Data.Word

    import Utils.Hex

    type Byte = Word8

    type Bytes = [Byte]

    newtype HexByte
        = HexByte Byte
        deriving (Eq, Ord, Bounded)

    instance Enum HexByte where
        toEnum value = HexByte $ toEnum value
        fromEnum (HexByte value) = fromEnum value

    instance Show HexByte where
        show (HexByte value) =
            toHexString value

    instance Read HexByte where
        readsPrec p value =
            map (\(x, y) -> (HexByte x, y)) $ readsPrec p value

    toWord8 :: HexByte -> Word8
    toWord8 (HexByte value) = value