module Chip8.Assembly.Compiling.Utilities where

    import Chip8.Assembly.Language
    import Chip8.Assembly.Compiling.State

    import Data.Word
    import Data.Bits

    writeWord :: Word16 -> Action
    writeWord word = do
        writeByte $ fromIntegral $ (shiftR word 8) .&. 0xFF
        writeByte $ fromIntegral $ (shiftR word 0) .&. 0xFF

    writeBytes :: Byte -> Byte -> Action
    writeBytes upper lower = do
        writeByte upper
        writeByte lower

    writeLabelInstruction :: Word8 -> Identifier -> Action
    writeLabelInstruction opcode label = do
        maybeTarget <- lookupLabel label
        case maybeTarget of
            Nothing -> do
                addError $ "Undefined label: " ++ show label

            Just target ->
                writeAddressInstruction opcode (addressOf target)

    writeAddressInstruction :: Word8 -> Address -> Action
    writeAddressInstruction opcode (Address address) = do
        let opcode' = shiftL (opcode .&. 0xF) 4
        let nibble = fromIntegral ((shiftR address 8) .&. 0xF)

        let upper = (opcode' .|. nibble)
        let lower = fromIntegral (address .&. 0xFF)

        writeBytes upper lower

    writeRegisterByteInstruction :: Word8 -> Register -> Byte -> Action
    writeRegisterByteInstruction opcode register byte = do
        let opcode' = shiftL (opcode .&. 0xF) 4
        let register' = fromIntegral $ shiftL ((fromEnum register) .&. 0xF) 0

        let upper = (opcode' .|. register')

        writeBytes upper byte

    writeRegisterRegisterInstruction :: Word8 -> Word8 -> Register -> Register -> Action
    writeRegisterRegisterInstruction opcode variant left right = do
        let opcode' = shiftL (opcode .&. 0xF) 4
        let left' = fromIntegral $ shiftL ((fromEnum left) .&. 0xF) 0
        let right' = fromIntegral $ shiftL ((fromEnum right) .&. 0xF) 4
        let variant' = shiftL (opcode .&. 0xF) 0

        let upper = (opcode' .|. left')
        let lower = (right' .|. variant')

        writeBytes upper lower

    processTargetedInstruction :: Identifier -> (Target -> Action) -> Action
    processTargetedInstruction label continuation = do
        maybeTarget <- lookupLabel label
        case maybeTarget of
            Nothing -> do
                address <- getAddress
                subscribeFixup label (address, continuation)
                -- Skip the word that would have been written to avoid an overwrite warning
                advanceAddress 2

            Just target ->
                continuation target

    unhandledTarget :: String -> Identifier -> Target -> Action
    unhandledTarget caller label target =
        error' ["Attempt to use ", caller, "on ", nameOf target ,": ", label']
        where
            label' = show label
            error' = addError . concat
