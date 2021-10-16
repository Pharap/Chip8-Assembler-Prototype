module Chip8.Assembly.Compiling.SecondPass (secondPass) where

    import Chip8.Assembly.Language
    import Chip8.Assembly.Compiling.State as CState
    import Chip8.Assembly.Compiling.Instructions
    import Chip8.Assembly.Compiling.Utilities

    secondPass :: [Statement] -> Action
    secondPass statements =
        sequence_ $ map processStatement statements

    processStatement :: Statement -> Action
    processStatement statement =
        case statement of
            Instruction instruction ->
                processInstruction instruction

            LabelDeclaration identifier ->
                processLabel identifier

            SystemDeclaration identifier address ->
                processSystem identifier address

            DataDefinition identifier bytes ->
                processData identifier bytes

            MacroDefinition identifier statements ->
                processMacro identifier statements

            FunctionDefinition identifier statements ->
                processFunction identifier statements

            BlockDefinition identifier statements ->
                processBlock identifier statements

            ExpandStatement identifier ->
                processExpand identifier

    processInstruction :: Instruction -> Action
    processInstruction instruction =
        case instruction of
            ClearScreen -> processClearScreen
            Return -> processReturn
            SystemCall identifier -> processSystemCall identifier
            Jump identifier -> processJump identifier
            Call identifier -> processCall identifier
            SkipIfEqualRegisterByte register byte -> processSkipIfEqualRegisterByte register byte
            SkipIfNotEqualRegisterByte register byte -> processSkipIfNotEqualRegisterByte register byte
            SkipIfEqualRegisterRegister left right -> processSkipIfEqualRegisterRegister left right
            LoadRegisterByte register byte -> processLoadRegisterByte register byte
            AddRegisterByte register byte -> processAddRegisterByte register byte
            LoadRegisterRegister left right -> processLoadRegisterRegister left right
            OrRegisterRegister left right -> processOrRegisterRegister left right
            AndRegisterRegister left right -> processAndRegisterRegister left right
            ExorRegisterRegister left right -> processExorRegisterRegister left right
            AddRegisterRegister left right -> processAddRegisterRegister left right
            SubtractRegisterRegister left right -> processSubtractRegisterRegister left right
            ShiftRightRegister register -> processShiftRightRegister register
            SubtractInverseRegisterRegister left right -> processSubtractInverseRegisterRegister left right
            ShiftLeftRegister register -> processShiftLeftRegister register
            SkipIfNotEqualRegisterRegister left right -> processSkipIfNotEqualRegisterRegister left right
            LoadIAddress identifier -> processLoadIAddress identifier
            JumpV0 identifier -> processJumpV0 identifier
            Random register byte -> processRandom register byte
            Draw left right byte -> processDraw left right byte
            SkipKeyPressed register -> processSkipKeyPressed register
            SkipKeyNotPressed register -> processSkipKeyNotPressed register
            LoadRegisterDelay register -> processLoadRegisterDelay register
            LoadRegisterKey register -> processLoadRegisterKey register
            LoadDelayRegister register -> processLoadDelayRegister register
            LoadSoundRegister register -> processLoadSoundRegister register
            AddIRegister register -> processAddIRegister register
            LoadFontGlyph register -> processLoadFontGlyph register
            LoadBCD register -> processLoadBCD register
            LoadMemoryRegisters register -> processLoadMemoryRegisters register
            LoadRegistersMemory register -> processLoadRegistersMemory register

    processLabel :: Identifier -> Action
    processLabel identifier =
        return ()

    processSystem :: Identifier -> Address -> Action
    processSystem identifier address =
        return ()

    processData :: Maybe Identifier -> Bytes -> Action
    processData identifier bytes = do
        sequence_ $ map writeByte bytes

    processMacro :: Identifier -> Statements -> Action
    processMacro identifier statements =
        return ()

    processFunction :: Identifier -> Statements -> Action
    processFunction identifier statements = do
        sequence_ $ map processStatement statements
        processInstruction Return

    processBlock :: Maybe Identifier -> Statements -> Action
    processBlock identifier statements = do
        sequence_ $ map processStatement statements

    processExpand :: Identifier -> Action
    processExpand label = do
        processTargetedInstruction label $ \target ->
            case target of
                Macro statements ->
                    sequence_ $ map processStatement statements

                _ ->
                    addError $ concat ["Attempt to expand ", nameOf target ,": ", show label]