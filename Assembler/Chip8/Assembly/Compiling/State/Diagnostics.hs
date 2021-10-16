module Chip8.Assembly.Compiling.State.Diagnostics where

    data Diagnostic
        = Error String
        | Warning String
        | Information String
        deriving (Eq, Ord, Show, Read)

    type Diagnostics = [Diagnostic]

    isError :: Diagnostic -> Bool
    isError (Error _) = True
    isError _ = False

    hasErrors :: Diagnostics -> Bool
    hasErrors =
        any isError

    getErrors :: Diagnostics -> Diagnostics
    getErrors =
        filter isError

    isWarning :: Diagnostic -> Bool
    isWarning (Warning _) = True
    isWarning _ = False

    hasWarning :: Diagnostics -> Bool
    hasWarning =
        any isWarning

    getWarnings :: Diagnostics -> Diagnostics
    getWarnings =
        filter isWarning

    isInformation :: Diagnostic -> Bool
    isInformation (Information _) = True
    isInformation _ = False

    hasInformation :: Diagnostics -> Bool
    hasInformation =
        any isInformation

    getInformation :: Diagnostics -> Diagnostics
    getInformation =
        filter isInformation