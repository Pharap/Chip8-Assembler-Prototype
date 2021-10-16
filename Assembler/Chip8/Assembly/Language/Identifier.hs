module Chip8.Assembly.Language.Identifier where

    newtype Identifier
        = Identifier String
        deriving (Eq, Ord)

    instance Show Identifier where
        show (Identifier value) = value