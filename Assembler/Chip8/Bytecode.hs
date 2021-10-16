module Chip8.Bytecode where
    
    newtype Address
        = Address Int
        deriving (Eq, Ord)

    instance Show Address where
        show (Address address) =
            '#':(show address)

    data Bytecode
        = SystemCall