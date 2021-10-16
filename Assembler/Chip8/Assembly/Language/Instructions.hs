module Chip8.Assembly.Language.Instructions where

    import Chip8.Assembly.Language.Byte
    import Chip8.Assembly.Language.Address
    import Chip8.Assembly.Language.Register
    import Chip8.Assembly.Language.Identifier

    data Instruction
        = ClearScreen
        | Return
        | SystemCall Identifier
        | Jump Identifier
        | Call Identifier
        | SkipIfEqualRegisterByte Register Byte
        | SkipIfNotEqualRegisterByte Register Byte
        | SkipIfEqualRegisterRegister Register Register
        | LoadRegisterByte Register Byte
        | AddRegisterByte Register Byte
        | LoadRegisterRegister Register Register
        | OrRegisterRegister Register Register
        | AndRegisterRegister Register Register
        | ExorRegisterRegister Register Register
        | AddRegisterRegister Register Register
        | SubtractRegisterRegister Register Register
        | ShiftRightRegister Register
        | SubtractInverseRegisterRegister Register Register
        | ShiftLeftRegister Register
        | SkipIfNotEqualRegisterRegister Register Register
        | LoadIAddress Identifier
        | JumpV0 Identifier
        | Random Register Byte
        | Draw Register Register Byte
        | SkipKeyPressed Register
        | SkipKeyNotPressed Register
        | LoadRegisterDelay Register
        | LoadRegisterKey Register
        | LoadDelayRegister Register
        | LoadSoundRegister Register
        | AddIRegister Register
        | LoadFontGlyph Register
        | LoadBCD Register
        | LoadMemoryRegisters Register
        | LoadRegistersMemory Register
        deriving (Eq, Ord, Show)

    type Instructions = [Instruction]