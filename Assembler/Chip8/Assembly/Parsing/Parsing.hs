{-# LANGUAGE FlexibleContexts #-}

module Chip8.Assembly.Parsing.Parsing where

    import Chip8.Assembly.Language (Statement)
    import Chip8.Assembly.Parsing.Statements (statements)

    import Text.Parsec

    file :: Stream s m Char => ParsecT s u m [Statement]
    file =
        between spaces eof statements