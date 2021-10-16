{-# LANGUAGE FlexibleContexts #-}

module Chip8.Assembly.Parsing.Basics where

    import Chip8.Assembly.Language

    import Text.Parsec hiding (label)

    import Control.Monad

    import Utils.Hex

    byte :: Stream s m Char => ParsecT s u m Byte
    byte =
        liftM fromIntegral integer

    address :: Stream s m Char => ParsecT s u m Address
    address =
        liftM (Address . fromIntegral) integer

    register :: Stream s m Char => ParsecT s u m Register
    register =
        char 'V' >> liftM (toEnum . intFromHex) (oneOf $ ['0'..'9'] ++ ['A'..'F'])

    identifier :: Stream s m Char => ParsecT s u m Identifier
    identifier =
        liftM Identifier $ liftM2 (:) initial remaining
        where
            initial = letter
            remaining = many (letter <|> digit <|> char '_')

    integer :: Stream s m Char => ParsecT s u m Int
    integer =
        hexadecimal <|> decimal
    
    decimal :: Stream s m Char => ParsecT s u m Int
    decimal =
        liftM read (many1 digit)
    
    hexadecimal :: Stream s m Char => ParsecT s u m Int
    hexadecimal =
        char '#' >>
        liftM (intFromHexString) (many1 hexDigit)

    whitespace :: Stream s m Char => ParsecT s u m Char
    whitespace = oneOf [' ', '\t', '\f', '\v']

    linespace :: Stream s m Char => ParsecT s u m Char
    linespace = oneOf [' ', '\t', '\f', '\v', '\r', '\n']

    optionalSpace :: Stream s m Char => ParsecT s u m ()
    optionalSpace = skipMany whitespace

    requiredSpace :: Stream s m Char => ParsecT s u m ()
    requiredSpace = skipMany1 whitespace

    optionalLinespace :: Stream s m Char => ParsecT s u m ()
    optionalLinespace = skipMany linespace

    requiredLinespace :: Stream s m Char => ParsecT s u m ()
    requiredLinespace = skipMany1 linespace

    quotedString :: Stream s m Char => ParsecT s u m String
    quotedString =
        liftM (read . bookend) $
        between (char '"') (char '"') $
        many1 (satisfy (/= '"'))
        where
            bookend s = "\"" ++ s ++ "\""