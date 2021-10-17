module Chip8.Assembly.Language.Primary where

    import Chip8.Assembly.Language.Byte
    import Chip8.Assembly.Language.Address
    import Chip8.Assembly.Language.Register
    import Chip8.Assembly.Language.Identifier
    import Chip8.Assembly.Language.Instructions

    data Statement
        = Instruction Instruction
        | LabelDeclaration Identifier
        | SystemDeclaration Identifier Address
        | DataDefinition (Maybe Identifier) Bytes
        | MacroDefinition Identifier Statements
        | FunctionDefinition Identifier Statements
        | BlockDefinition (Maybe Identifier) Statements
        | LoopDefinition Statements
        | ExpandStatement Identifier
        deriving (Eq, Ord, Show)

    type Statements = [Statement]

    data Target
        = Label Address
        | System Address
        | Function Address
        | Data Address
        | Macro Statements
        deriving (Eq, Ord, Show)

    nameOf :: Target -> String
    nameOf target =
        case target of
            Label _ -> "label"
            System _ -> "system function"
            Function _ -> "function"
            Data _ -> "data"
            Macro _ -> "macro"

    addressOf :: Target -> Address
    addressOf target =
        case target of
            Label address -> address
            System address -> address
            Function address -> address
            Data address -> address
            Macro _ -> error "Macros do not have addresses"

    maybeAddressOf :: Target -> Maybe Address
    maybeAddressOf target =
        case target of
            Label address -> Just address
            System address -> Just address
            Function address -> Just address
            Data address -> Just address
            Macro _ -> Nothing