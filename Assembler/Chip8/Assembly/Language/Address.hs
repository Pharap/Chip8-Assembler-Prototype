module Chip8.Assembly.Language.Address where

    import Data.Word
    import Data.Ix

    import Utils.Hex

    newtype Address
        = Address Word16
        deriving (Eq, Ord)

    instance Bounded Address where
        minBound = Address 0x000
        maxBound = Address 0xFFF

    instance Enum Address where
        toEnum value = Address $ toEnum value
        fromEnum (Address value) = fromEnum value

    instance Show Address where
        show (Address value) = '#':(toHexString value)

    instance Read Address where
        readsPrec p value = map (\(x, y) -> (Address x, y)) $ readsPrec p value

    instance Ix Address where
        range (Address l, Address r) =
            map Address $ range (l, r)

        index (Address l, Address r) (Address subcript) =
            index (l, r) subcript

        inRange (Address l, Address r) (Address subcript) =
            inRange (l, r) subcript

        rangeSize (Address l, Address r) =
            rangeSize (l, r)

    offsetBy :: Int -> Address -> Address
    offsetBy offset (Address address) =
        Address $ fromIntegral $ (+) (fromIntegral address) offset