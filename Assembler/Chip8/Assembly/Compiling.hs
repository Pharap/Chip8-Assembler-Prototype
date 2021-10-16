module Chip8.Assembly.Compiling where

    import Chip8.Assembly.Language
    import Chip8.Assembly.Compiling.State as CState
    import Chip8.Assembly.Compiling.FirstPass (firstPass)
    import Chip8.Assembly.Compiling.SecondPass (secondPass)

    import Control.Monad
    import Control.Monad.State.Lazy

    import Data.ByteString.Lazy
    import Data.ByteString.Builder

    data Result =
        Result ByteString Diagnostics
        deriving (Eq, Show)

    getBytes :: Result -> ByteString
    getBytes (Result bytes _) = bytes

    getDiagnostics :: Result -> Diagnostics
    getDiagnostics (Result _ diagnostics) = diagnostics

    compile :: [Statement] -> Result
    compile statements =
        evalState (compileState statements) standardState

    compileState :: [Statement] -> Query Result
    compileState statements =
        firstPass statements >>
        secondPass statements >>
        liftM2 Result
            (liftM buildMemory' (gets memory))
            (gets diagnostics)
        where
            buildMemory' =
                toLazyByteString . buildMemory (Address 0x200, Address 0xFFF)