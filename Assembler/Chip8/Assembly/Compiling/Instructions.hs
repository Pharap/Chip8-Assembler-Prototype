module Chip8.Assembly.Compiling.Instructions where

    import Chip8.Assembly.Language
    import Chip8.Assembly.Compiling.State as CState
    import Chip8.Assembly.Compiling.Utilities

    import Control.Monad.State.Lazy

    processClearScreen :: Action
    processClearScreen =
        writeWord 0x00E0

    processReturn :: Action
    processReturn =
        writeWord 0x00EE

    processSystemCall :: Identifier -> Action
    processSystemCall label =
        processTargetedInstruction label $ \target ->
            case target of
                System address -> do
                    when (address > (Address 0x1FF)) $
                        addError $ concat [
                            "Attempt to use SYS beyond 0x1FF: ",
                            show label, " ", show address
                        ]
                    writeAddressInstruction 0x0 address
                    
                _ ->
                    unhandledTarget "SYS" label target

    processJump :: Identifier -> Action
    processJump label =
        processTargetedInstruction label $ \target ->
            case target of
                Label address -> do
                    when (address < (Address 0x200)) $
                        addError $ concat [
                            "Attempt to use JP below 0x200: ",
                            show label, " ", show address
                        ]
                    writeAddressInstruction 0x1 address
                    
                _ ->
                    unhandledTarget "JP" label target

    processCall :: Identifier -> Action
    processCall label =
        processTargetedInstruction label $ \target ->
            case target of
                Function address -> do
                    when (address < (Address 0x200)) $
                        addError $ concat [
                            "Attempt to use CALL below 0x200: ",
                            show label, " ", show address
                        ]
                    writeAddressInstruction 0x2 address
                    
                _ ->
                    unhandledTarget "CALL" label target

    processSkipIfEqualRegisterByte :: Register -> Byte -> Action
    processSkipIfEqualRegisterByte =
        writeRegisterByteInstruction 0x3

    processSkipIfNotEqualRegisterByte :: Register -> Byte -> Action
    processSkipIfNotEqualRegisterByte =
        writeRegisterByteInstruction 0x4

    processSkipIfEqualRegisterRegister :: Register -> Register -> Action
    processSkipIfEqualRegisterRegister =
        writeRegisterRegisterInstruction 0x5 0x0

    processLoadRegisterByte :: Register -> Byte -> Action
    processLoadRegisterByte =
        writeRegisterByteInstruction 0x6

    processAddRegisterByte :: Register -> Byte -> Action
    processAddRegisterByte =
        writeRegisterByteInstruction 0x7

    processLoadRegisterRegister :: Register -> Register -> Action
    processLoadRegisterRegister =
        writeRegisterRegisterInstruction 0x8 0x0

    processOrRegisterRegister :: Register -> Register -> Action
    processOrRegisterRegister =
        writeRegisterRegisterInstruction 0x8 0x1

    processAndRegisterRegister :: Register -> Register -> Action
    processAndRegisterRegister =
        writeRegisterRegisterInstruction 0x8 0x2

    processExorRegisterRegister :: Register -> Register -> Action
    processExorRegisterRegister =
        writeRegisterRegisterInstruction 0x8 0x3

    processAddRegisterRegister :: Register -> Register -> Action
    processAddRegisterRegister =
        writeRegisterRegisterInstruction 0x8 0x4

    processSubtractRegisterRegister :: Register -> Register -> Action
    processSubtractRegisterRegister =
        writeRegisterRegisterInstruction 0x8 0x5

    processShiftRightRegister :: Register -> Action
    processShiftRightRegister register =
        writeRegisterRegisterInstruction 0x8 0x6 register V0

    processSubtractInverseRegisterRegister :: Register -> Register -> Action
    processSubtractInverseRegisterRegister =
        writeRegisterRegisterInstruction 0x8 0x7

    processShiftLeftRegister :: Register -> Action
    processShiftLeftRegister register =
        writeRegisterRegisterInstruction 0x8 0xE register V0

    processSkipIfNotEqualRegisterRegister :: Register -> Register -> Action
    processSkipIfNotEqualRegisterRegister =
        writeRegisterRegisterInstruction 0x9 0x0

    processLoadIAddress :: Identifier -> Action
    processLoadIAddress =
        writeLabelInstruction 0xA

    processJumpV0 :: Identifier -> Action
    processJumpV0 =
        writeLabelInstruction 0xB

    processRandom :: Register -> Byte -> Action
    processRandom =
        writeRegisterByteInstruction 0xC

    processDraw :: Register -> Register -> Byte -> Action
    processDraw left right nibble =
        writeRegisterRegisterInstruction 0x9 nibble left right

    processSkipKeyPressed :: Register -> Action
    processSkipKeyPressed register =
        writeRegisterByteInstruction 0xE register 0x9E

    processSkipKeyNotPressed :: Register -> Action
    processSkipKeyNotPressed register =
        writeRegisterByteInstruction 0xE register 0xA1

    processLoadRegisterDelay :: Register -> Action
    processLoadRegisterDelay register =
        writeRegisterByteInstruction 0xF register 0x07

    processLoadRegisterKey :: Register -> Action
    processLoadRegisterKey register =
        writeRegisterByteInstruction 0xF register 0x0A

    processLoadDelayRegister :: Register -> Action
    processLoadDelayRegister register =
        writeRegisterByteInstruction 0xF register 0x15

    processLoadSoundRegister :: Register -> Action
    processLoadSoundRegister register =
        writeRegisterByteInstruction 0xF register 0x18

    processAddIRegister :: Register -> Action
    processAddIRegister register =
        writeRegisterByteInstruction 0xF register 0x1E

    processLoadFontGlyph :: Register -> Action
    processLoadFontGlyph register =
        writeRegisterByteInstruction 0xF register 0x29

    processLoadBCD :: Register -> Action
    processLoadBCD register =
        writeRegisterByteInstruction 0xF register 0x33

    processLoadMemoryRegisters :: Register -> Action
    processLoadMemoryRegisters register =
        writeRegisterByteInstruction 0xF register 0x55

    processLoadRegistersMemory :: Register -> Action
    processLoadRegistersMemory register =
        writeRegisterByteInstruction 0xF register 0x65
