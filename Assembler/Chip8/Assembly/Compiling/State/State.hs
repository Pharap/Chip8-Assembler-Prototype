module Chip8.Assembly.Compiling.State.State where

    import Chip8.Assembly.Language
    import Chip8.Assembly.Compiling.State.Diagnostics

    import Data.Ix
    import Data.Bits
    import Data.Maybe
    import Data.Map as Map
    import Data.ByteString.Builder

    import Control.Monad
    import Control.Monad.State.Lazy

    type Query = State CompilationState

    type Action = State CompilationState ()

    type Memory = Map Address Byte

    type Labels = Map Identifier Target

    type Fixup = (Address, (Target -> Action))

    type Fixups = Map Identifier [Fixup]

    data CompilationState
        = CompilationState {
            memory :: Memory,
            labels :: Labels,
            fixups :: Fixups,
            address :: Address,
            labelId :: Int,
            diagnostics :: Diagnostics
        }

    standardState :: CompilationState
    standardState =
        CompilationState {
            memory = Map.empty,
            labels = Map.empty,
            fixups = Map.empty,
            address = Address 0x200,
            labelId = 0,
            diagnostics = []
        }

    --
    -- diagnostics
    --

    getDiagnostics :: Query Diagnostics
    getDiagnostics =
        liftM diagnostics get

    setDiagnostics :: Diagnostics -> Action
    setDiagnostics newDiagnostics =
        modify $ \state ->
            state { diagnostics = newDiagnostics }

    modifyDiagnostics :: (Diagnostics -> Diagnostics) -> Action
    modifyDiagnostics f =
        modify $ \state ->
            state { diagnostics = f (diagnostics state) }

    addDiagnostic :: Diagnostic -> Action
    addDiagnostic diagnostic =
        modifyDiagnostics $ (diagnostic:)

    addError :: String -> Action
    addError message =
        addDiagnostic $ Error message

    addWarning :: String -> Action
    addWarning message =
        addDiagnostic $ Warning message

    addInformation :: String -> Action
    addInformation message =
        addDiagnostic $ Information message

    --
    -- labels
    --

    getLabels :: Query Labels
    getLabels =
        gets labels

    setLabels :: Labels -> Action
    setLabels newLabels =
        modify $ \state ->
            state { labels = newLabels }

    modifyLabels :: (Labels -> Labels) -> Action
    modifyLabels f =
        modify $ \state ->
            state { labels = f (labels state) }

    lookupLabel :: Identifier -> Query (Maybe Target)
    lookupLabel label =
        gets (Map.lookup label . labels)

    labelExists :: Identifier -> Query Bool
    labelExists label =
        gets (Map.member label . labels)

    addLabelUnsafe :: Identifier -> Target -> Action
    addLabelUnsafe label target =
        modifyLabels $ insert label target

    addLabel :: Identifier -> Target -> Action
    addLabel label target = do
        exists <- labelExists label
        when exists (addError $ concat ["Label overwritten: ", show label])
        addLabelUnsafe label target

    deleteLabel :: Identifier -> Action
    deleteLabel label =
        modifyLabels (Map.delete label)

    generateLabel :: Query Identifier
    generateLabel = do
        current <- gets labelId
        modify $ \state ->
            state { labelId = (current + 1) }
        return $ Identifier $ '@':(show current)

    generateLabelFor :: Target -> Query Identifier
    generateLabelFor target = do
        label <- generateLabel
        addLabel label target
        return label

    fixupLabel :: Identifier -> Target -> Action
    fixupLabel label target = do
        addLabel label target
        maybeFixups <- lookupFixup label
        case maybeFixups of
            Nothing ->
                return()

            Just fixups ->
                sequence_ $ Prelude.map f fixups
        where
            f :: Fixup -> Action
            f (address, action) =
                runAtAddress address (action target)

    --
    -- fixups
    --

    getFixups :: Query Fixups
    getFixups =
        gets fixups

    setFixups :: Fixups -> Action
    setFixups newFixups =
        modify $ \state ->
            state { fixups = newFixups }

    modifyFixups :: (Fixups -> Fixups) -> Action
    modifyFixups f =
        modify $ \state ->
            state { fixups = f (fixups state) }

    lookupFixup :: Identifier -> Query (Maybe [Fixup])
    lookupFixup label =
        gets (Map.lookup label . fixups)

    fixupExists :: Identifier -> Query Bool
    fixupExists label =
        gets (Map.member label . fixups)

    addFixup :: Identifier -> Action
    addFixup label =
        modifyFixups $ Map.alter update' label
        where
            update' :: Maybe [a] -> Maybe [a]
            update' = maybe (Just []) Just

    subscribeFixup :: Identifier -> Fixup -> Action
    subscribeFixup label fixup = do
        fixups <- lookupFixup label
        case fixups of
            Nothing ->
                modifyFixups $ insert label [fixup]

            Just existing ->
                modifyFixups $ insert label (fixup:existing)

    deleteFixup :: Identifier -> Action
    deleteFixup label =
        modifyFixups (Map.delete label)

    --
    -- address
    --

    getAddress :: Query Address
    getAddress =
        liftM address get

    setAddress :: Address -> Action
    setAddress newAddress =
        modify $ \state ->
            state { address = newAddress }

    modifyAddress :: (Address -> Address) -> Action
    modifyAddress f =
        modify $ \state ->
            state { address = f (address state) }

    advanceAddress :: Int -> Action
    advanceAddress amount =
        modifyAddress $ offsetBy amount

    runAtAddress :: Address -> Action -> Action
    runAtAddress address action = do
        current <- getAddress
        setAddress address
        action
        setAddress current

    --
    -- memory
    --

    getMemory :: Query Memory
    getMemory =
        gets memory

    setMemory :: Memory -> Action
    setMemory newMemory =
        modify $ \state ->
            state { memory = newMemory }

    modifyMemory :: (Memory -> Memory) -> Action
    modifyMemory f =
        modify $ \state ->
            state { memory = f (memory state) }

    setByteUnsafe :: Address -> Byte -> Action
    setByteUnsafe address byte =
        modifyMemory $ insert address byte

    setByte :: Address -> Byte -> Action
    setByte address byte = do
        exists <- existsByte address
        when exists (addWarning "Byte overwritten via setByte")
        setByteUnsafe address byte

    getByte :: Address -> Query Byte
    getByte address =
        gets (Map.findWithDefault 0 address . memory)

    lookupByte :: Address -> Query (Maybe Byte)
    lookupByte address =
        gets (Map.lookup address . memory)

    existsByte :: Address -> Query Bool
    existsByte address =
        gets (Map.member address . memory)

    writeByteUnsafe :: Byte -> Action
    writeByteUnsafe byte = do
        address <- getAddress
        setByteUnsafe address byte
        advanceAddress 1

    writeByte :: Byte -> Action
    writeByte byte = do
        address <- getAddress
        exists <- existsByte address
        when exists (addWarning "Byte overwritten via writeByte")
        setByteUnsafe address byte
        advanceAddress 1

    buildMemory :: (Address, Address) -> Memory -> Builder
    buildMemory (lower, upper) memory =
        mconcat $ Prelude.map resolve $ range (lower, upper)
        where
            resolve address =
                word8 $ Map.findWithDefault 0 address memory