module Chip8.Assembly.Compiling.FirstPass (firstPass) where

    import Chip8.Assembly.Language
    import Chip8.Assembly.Compiling.State

    firstPass :: [Statement] -> Action
    firstPass statements =
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

            LoopDefinition statements ->
                processLoop statements

            ExpandStatement identifier ->
                processExpand identifier

    processInstruction :: Instruction -> Action
    processInstruction instruction =
        return ()

    processLabel :: Identifier -> Action
    processLabel identifier = do
        addFixup identifier

    processSystem :: Identifier -> Address -> Action
    processSystem identifier address =
        addLabel identifier (System address)

    processData :: Maybe Identifier -> Bytes -> Action
    processData identifier bytes =
        maybe (return ()) addFixup identifier

    processMacro :: Identifier -> Statements -> Action
    processMacro identifier statements =
        addLabel identifier (Macro statements)

    processFunction :: Identifier -> Statements -> Action
    processFunction identifier statements = do
        addFixup identifier
        sequence_ $ map processStatement statements

    processBlock :: Maybe Identifier -> Statements -> Action
    processBlock identifier statements = do
        maybe (return ()) addFixup identifier
        sequence_ $ map processStatement statements

    processLoop :: Statements -> Action
    processLoop statements =
        sequence_ $ map processStatement statements

    processExpand :: Identifier -> Action
    processExpand identifier =
        return ()