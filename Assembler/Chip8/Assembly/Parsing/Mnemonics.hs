{-# LANGUAGE FlexibleContexts #-}

module Chip8.Assembly.Parsing.Mnemonics where

    import Chip8.Assembly.Language
    import Chip8.Assembly.Parsing.Basics

    import Text.Parsec hiding (label)

    import Control.Monad

    parseCLS :: Stream s m Char => ParsecT s u m Instruction
    parseCLS = do
        string "CLS"
        return ClearScreen

    parseRET :: Stream s m Char => ParsecT s u m Instruction
    parseRET = do
        string "RET"
        return Return

    parseSYS :: Stream s m Char => ParsecT s u m Instruction
    parseSYS = do
        string "SYS"
        requiredSpace
        liftM SystemCall identifier

    parseJP :: Stream s m Char => ParsecT s u m Instruction
    parseJP = do
        string "JP"
        requiredSpace
        choice $ map try [
                jumpV0,
                jump
            ]
        where
            jump :: Stream s m Char => ParsecT s u m Instruction
            jump =
                liftM Jump identifier

            jumpV0 :: Stream s m Char => ParsecT s u m Instruction
            jumpV0 = do
                string "V0"
                requiredSpace
                liftM JumpV0 identifier

    parseCALL :: Stream s m Char => ParsecT s u m Instruction
    parseCALL = do
        string "CALL"
        requiredSpace
        liftM Call identifier

    parseSE :: Stream s m Char => ParsecT s u m Instruction
    parseSE = do
        string "SE"
        requiredSpace
        choice $ map try [
                skipEqualRegisterByte,
                skipEqualRegisterRegister
            ]
        where
            skipEqualRegisterByte :: Stream s m Char => ParsecT s u m Instruction
            skipEqualRegisterByte = do
                l <- register
                requiredSpace
                r <- byte
                return (SkipIfEqualRegisterByte l r)

            skipEqualRegisterRegister :: Stream s m Char => ParsecT s u m Instruction
            skipEqualRegisterRegister = do
                l <- register
                requiredSpace
                r <- register
                return (SkipIfEqualRegisterRegister l r)

    parseSNE :: Stream s m Char => ParsecT s u m Instruction
    parseSNE = do
        string "SNE"
        requiredSpace
        choice $ map try [
                skipNotEqualRegisterByte,
                skipNotEqualRegisterRegister
            ]
        where
            skipNotEqualRegisterByte :: Stream s m Char => ParsecT s u m Instruction
            skipNotEqualRegisterByte = do
                l <- register
                requiredSpace
                r <- byte
                return (SkipIfNotEqualRegisterByte l r)

            skipNotEqualRegisterRegister :: Stream s m Char => ParsecT s u m Instruction
            skipNotEqualRegisterRegister = do
                l <- register
                requiredSpace
                r <- register
                return (SkipIfNotEqualRegisterRegister l r)

    parseLD :: Stream s m Char => ParsecT s u m Instruction
    parseLD = do
        string "LD"
        requiredSpace
        choice $ map try [
                loadRegisterByte,
                loadRegisterRegister,
                loadIAddress,
                loadRegisterDelay,
                loadRegisterKey,
                loadDelayRegister,
                loadSoundRegister,
                loadFontGlyph,
                loadBCD,
                loadMemoryRegisters,
                loadRegistersMemory
            ]
        where
            loadRegisterByte :: Stream s m Char => ParsecT s u m Instruction
            loadRegisterByte = do
                l <- register
                requiredSpace
                r <- byte
                return (LoadRegisterByte l r)

            loadRegisterRegister :: Stream s m Char => ParsecT s u m Instruction
            loadRegisterRegister = do
                l <- register
                requiredSpace
                r <- register
                return (LoadRegisterRegister l r)

            loadIAddress :: Stream s m Char => ParsecT s u m Instruction
            loadIAddress = do
                string "I"
                requiredSpace
                r <- identifier
                return (LoadIAddress r)

            loadRegisterDelay :: Stream s m Char => ParsecT s u m Instruction
            loadRegisterDelay = do
                l <- register
                requiredSpace
                string "DT"
                return (LoadRegisterDelay l)

            loadRegisterKey :: Stream s m Char => ParsecT s u m Instruction
            loadRegisterKey = do
                l <- register
                requiredSpace
                string "K"
                return (LoadRegisterKey l)

            loadDelayRegister :: Stream s m Char => ParsecT s u m Instruction
            loadDelayRegister = do
                string "DT"
                requiredSpace
                r <- register
                return (LoadDelayRegister r)

            loadSoundRegister :: Stream s m Char => ParsecT s u m Instruction
            loadSoundRegister = do
                string "ST"
                requiredSpace
                r <- register
                return (LoadSoundRegister r)

            loadFontGlyph :: Stream s m Char => ParsecT s u m Instruction
            loadFontGlyph = do
                string "F"
                requiredSpace
                r <- register
                return (LoadFontGlyph r)

            loadBCD :: Stream s m Char => ParsecT s u m Instruction
            loadBCD = do
                string "B"
                requiredSpace
                r <- register
                return (LoadBCD r)

            loadMemoryRegisters :: Stream s m Char => ParsecT s u m Instruction
            loadMemoryRegisters = do
                string "[I]"
                requiredSpace
                r <- register
                return (LoadMemoryRegisters r)

            loadRegistersMemory :: Stream s m Char => ParsecT s u m Instruction
            loadRegistersMemory = do
                l <- register
                requiredSpace
                string "[I]"
                return (LoadRegistersMemory l)

    parseADD :: Stream s m Char => ParsecT s u m Instruction
    parseADD = do
        string "ADD"
        requiredSpace
        choice $ map try [
                addRegisterByte,
                addRegisterRegister,
                addIRegister
            ]
        where
            addRegisterByte :: Stream s m Char => ParsecT s u m Instruction
            addRegisterByte = do
                l <- register
                requiredSpace
                r <- byte
                return (AddRegisterByte l r)

            addRegisterRegister :: Stream s m Char => ParsecT s u m Instruction
            addRegisterRegister = do
                l <- register
                requiredSpace
                r <- register
                return (AddRegisterRegister l r)

            addIRegister :: Stream s m Char => ParsecT s u m Instruction
            addIRegister = do
                string "I"
                requiredSpace
                r <- register
                return (AddIRegister r)

    parseOR :: Stream s m Char => ParsecT s u m Instruction
    parseOR = do
        string "OR"
        requiredSpace
        l <- register
        requiredSpace
        r <- register
        return (OrRegisterRegister l r)

    parseAND :: Stream s m Char => ParsecT s u m Instruction
    parseAND = do
        string "AND"
        requiredSpace
        l <- register
        requiredSpace
        r <- register
        return (AndRegisterRegister l r)

    parseXOR :: Stream s m Char => ParsecT s u m Instruction
    parseXOR = do
        string "XOR"
        requiredSpace
        l <- register
        requiredSpace
        r <- register
        return (ExorRegisterRegister l r)

    parseSUB :: Stream s m Char => ParsecT s u m Instruction
    parseSUB = do
        string "SUB"
        requiredSpace
        l <- register
        requiredSpace
        r <- register
        return (SubtractRegisterRegister l r)

    parseSHR :: Stream s m Char => ParsecT s u m Instruction
    parseSHR = do
        string "SHR"
        requiredSpace
        l <- register
        return (ShiftRightRegister l)

    parseSUBN :: Stream s m Char => ParsecT s u m Instruction
    parseSUBN = do
        string "SUBN"
        requiredSpace
        l <- register
        requiredSpace
        r <- register
        return (SubtractInverseRegisterRegister l r)

    parseSHL :: Stream s m Char => ParsecT s u m Instruction
    parseSHL = do
        string "SHL"
        requiredSpace
        l <- register
        return (ShiftLeftRegister l)

    parseRND :: Stream s m Char => ParsecT s u m Instruction
    parseRND = do
        string "RND"
        requiredSpace
        l <- register
        requiredSpace
        r <- byte
        return (Random l r)

    parseDRW :: Stream s m Char => ParsecT s u m Instruction
    parseDRW = do
        string "DRW"
        requiredSpace
        l <- register
        requiredSpace
        r <- register
        requiredSpace
        n <- byte
        return (Draw l r n)

        -- | SkipKeyPressed Register

        -- | SkipKeyNotPressed Register