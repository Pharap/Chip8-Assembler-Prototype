{-# LANGUAGE FlexibleContexts #-}

module Chip8.Assembly.Parsing.Statements (statements) where

    import Chip8.Assembly.Language
    import Chip8.Assembly.Parsing.Basics
    import Chip8.Assembly.Parsing.Mnemonics

    import Text.Parsec

    import Control.Monad

    import Data.Maybe
    import Data.Char

    statements :: Stream s m Char => ParsecT s u m [Statement]
    statements =
        liftM catMaybes $
        sepBy (optionMaybe topLevelStatement) (optionalSpace >> endOfLine)

    topLevelStatement :: Stream s m Char => ParsecT s u m Statement
    topLevelStatement =
        optionalSpace >>
        choice [
            systemStatement,
            expandStatement,
            macroStatement,
            functionStatement,
            asciiStatement,
            dataStatement,
            blockStatement,
            labelStatement,
            instructionStatement
        ]

    nestedStatement :: Stream s m Char => ParsecT s u m Statement
    nestedStatement =
        optionalSpace >>
        choice [
            expandStatement,
            blockStatement,
            labelStatement,
            instructionStatement
        ]

    systemStatement :: Stream s m Char => ParsecT s u m Statement
    systemStatement =
        string "system" >>
        liftM2 SystemDeclaration
        (requiredSpace >> identifier)
        (requiredSpace >> address)

    expandStatement :: Stream s m Char => ParsecT s u m Statement
    expandStatement =
        string "expand" >>
        liftM ExpandStatement (requiredSpace >> identifier)

    macroStatement :: Stream s m Char => ParsecT s u m Statement
    macroStatement = do
        string "macro"
        requiredSpace
        name <- identifier
        optionalLinespace
        char '{'
        optionalLinespace
        s <- statements'
        optionalLinespace
        char '}'
        return $ MacroDefinition name s
        where
            statements' :: Stream s m Char => ParsecT s u m [Statement]
            statements' =
                liftM catMaybes $
                sepBy (optionMaybe instructionStatement) endStatement

            endStatement :: Stream s m Char => ParsecT s u m Char
            endStatement =
                try $ between optionalSpace optionalLinespace (char ';' <|> endOfLine)

    functionStatement :: Stream s m Char => ParsecT s u m Statement
    functionStatement = do
        string "function"
        requiredSpace
        l <- identifier
        optionalLinespace
        char '{'
        optionalLinespace
        s <- statements'
        optionalLinespace
        char '}'
        return $ FunctionDefinition l s
        where
            statements' :: Stream s m Char => ParsecT s u m [Statement]
            statements' =
                liftM catMaybes $
                sepBy (optionMaybe nestedStatement) (optionalSpace >> endOfLine)

    asciiStatement :: Stream s m Char => ParsecT s u m Statement
    asciiStatement = do
        string "ascii"
        l <- optionMaybe (try $ requiredSpace >> identifier)
        requiredSpace
        s <- quotedString
        when (any (not . isAscii) s) (fail "Non-ascii character in string")
        let b = map (toEnum . fromEnum) s
        return $ DataDefinition l b

    dataStatement :: Stream s m Char => ParsecT s u m Statement
    dataStatement = do
        string "data"
        l <- optionMaybe (requiredSpace >> identifier)
        optionalLinespace
        char '{'
        optionalLinespace
        b <- endBy1 byte requiredLinespace
        optionalLinespace
        char '}'
        return $ DataDefinition l b

    blockStatement :: Stream s m Char => ParsecT s u m Statement
    blockStatement = do
        string "block"
        l <- optionMaybe (requiredSpace >> identifier)
        optionalLinespace
        char '{'
        optionalLinespace
        s <- statements'
        optionalLinespace
        char '}'
        return $ BlockDefinition l s
        where
            statements' :: Stream s m Char => ParsecT s u m [Statement]
            statements' =
                liftM catMaybes $
                sepBy (optionMaybe nestedStatement) (optionalSpace >> endOfLine)

    labelStatement :: Stream s m Char => ParsecT s u m Statement
    labelStatement =
        liftM LabelDeclaration $
        between (char ':') (char ':') identifier

    instructionStatement :: Stream s m Char => ParsecT s u m Statement
    instructionStatement =
        liftM Instruction instruction

    instruction :: Stream s m Char => ParsecT s u m Instruction
    instruction =
        choice $ map try [
            parseCLS,
            parseRET,
            parseSYS,
            parseJP,
            parseCALL,
            parseSE,
            parseSNE,
            parseLD,
            parseADD,
            parseOR,
            parseAND,
            parseXOR,
            parseSHR,
            parseSUBN,
            parseSHL,
            parseRND,
            parseDRW
        ]