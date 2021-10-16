module Chip8.Assembly.Language.Register where

    data Register
        = V0 | V1 | V2 | V3
        | V4 | V5 | V6 | V7
        | V8 | V9 | VA | VB
        | VC | VD | VE | VF
        deriving (Eq, Ord, Bounded, Enum, Show, Read)