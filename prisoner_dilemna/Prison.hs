{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE EmptyDataDecls        #-}


module Prison where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import qualified Data.Map                     as Map
import           GHC.Generics                 (Generic)
import qualified Data.OpenApi.Schema          as OpenApi
import           Ledger                       hiding (singleton)
import           Ledger.Value
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import           Playground.Contract          (ToSchema)
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)          
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude


data Prison = Prison
    { pOne                :: !PaymentPubKeyHash
    , pTwo                :: !PaymentPubKeyHash
    , pConstable          :: !PaymentPubKeyHash
    , pFine               :: !Integer
    , pOnePenalty         :: !(Integer, Integer)
    , pTwoPenalty         :: !(Integer, Integer)
    , pConfessionDeadline :: !POSIXTime
    , pSentencingDeadline :: !POSIXTime
    , pToken              :: !AssetClass 
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Prison

data PrisonerChoice = Defect | Cooperate 
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)
    

instance Eq PrisonerChoice where 
    Defect    == Defect    = True 
    Cooperate == Cooperate = True 
    _         == _         = False    

PlutusTx.unstableMakeIsData ''PrisonerChoice 

{-# INLINABLE sentence #-}
sentence :: PrisonerChoice -> PrisonerChoice -> (Integer, Integer)
sentence Defect Cooperate    = (10, 5)
sentence Cooperate Defect    = (5, 10)
sentence Defect Defect       = (3, 3)
sentence Cooperate Cooperate = (15, 15)


data PrisonDatum = PrisonDatum BuiltinByteString (Maybe PrisonerChoice) | Finished 
    deriving Show

instance Eq PrisonDatum where 
    PrisonDatum bs mc == PrisonDatum bs' mc' = (bs == bs') && (mc == mc')
    Finished          == Finished            = True 
    _                 == _                   = False 

PlutusTx.unstableMakeIsData ''PrisonDatum

data PrisonRedeemer = Action PrisonerChoice | Reveal BuiltinByteString  | PostBailOne | PostBailTwo
    deriving Show 

PlutusTx.unstableMakeIsData ''PrisonRedeemer 

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer 
lovelaces = Ada.getLovelace . Ada.fromValue 

{-# INLINABLE prisonDatum #-}
prisonDatum :: TxOut -> TxInfo -> (DatumHash -> TxInfo -> Maybe Datum) -> Maybe PrisonDatum 
prisonDatum o i f = do 
    dh       <- txOutDatum o 
    Datum d  <- f dh i 
    PlutusTx.fromBuiltinData d 

{-# INLINABLE prisonDatum' #-}
prisonDatum' :: Maybe Datum -> Maybe PrisonDatum 
prisonDatum' d = do  
    Datum d  <- d 
    PlutusTx.fromBuiltinData d

-- prisonDatum'' :: Datum -> Maybe PrisonDatum
-- prisonDatum'' d = \d -> PlutusTx.fromBuiltinData (Datum d)

-- Think also how one can extend this to other methods other than confessions... see notes on the courte system that uses a specific redeemer for providing proof
-- of something e.g that they have or have not done something that another person or system has said they have done. Can also be used in thoth for settling 
-- 'similarity' claims etc. (also similarity here can have a weird notion (see the yoneda lemma))


{-# INLINABLE mkPrisonValidator #-}
mkPrisonValidator :: Prison -> BuiltinByteString -> BuiltinByteString -> PrisonDatum -> PrisonRedeemer -> ScriptContext -> Bool
mkPrisonValidator prison bsDefect' bsCooperate' dat red ctx = 
    traceIfFalse "identifier token missing from input" (assetClassValueOf (txOutValue ownInput) (pToken prison) == 1) &&
    case (dat, red) of 
        (PrisonDatum bs Nothing , Action c) ->
            traceIfFalse "not signed by second prisoner"   (txSignedBy info (unPaymentPubKeyHash (pTwo prison)))                      && 
            traceIfFalse "first's prisoner fine missing"   (lovelaces (txOutValue ownInput) == pFine prison)                          && 
            traceIfFalse "second prisoner fine missing"    (lovelaces (txOutValue ownOutput) == 2 * pFine prison)                     &&
            traceIfFalse "wrong output datum"              (outputDatum == PrisonDatum bs (Just c))                                   &&
            traceIfFalse "missed deadline"                 (to (pConfessionDeadline prison) `contains` txInfoValidRange info)         &&
            traceIfFalse "token missing from output"       (assetClassValueOf (txOutValue ownOutput) (pToken prison) == 1)

        (PrisonDatum bs Nothing , Action c) ->
            traceIfFalse "not signed by second prisoner"   (txSignedBy info (unPaymentPubKeyHash (pTwo prison)))                      && 
            traceIfFalse "first's prisoner fine missing"   (lovelaces (txOutValue ownInput) == pFine prison)                          && 
            traceIfFalse "second prisoner fine missing"    (lovelaces (txOutValue ownOutput) == 2 * pFine prison)                     &&
            traceIfFalse "wrong output datum"              (outputDatum == PrisonDatum bs (Just c))                                   &&
            traceIfFalse "missed deadline"                 (to (pConfessionDeadline prison) `contains` txInfoValidRange info)         &&
            traceIfFalse "token missing from output"       (assetClassValueOf (txOutValue ownOutput) (pToken prison) == 1)     

        (PrisonDatum bs (Just c), Reveal nonce) ->  
            traceIfFalse "not signed by first prisoner"    (txSignedBy info (unPaymentPubKeyHash (pOne prison)))                      &&
            traceIfFalse "commit mismatch"                 (checkNonce bs nonce c)                                                    && 
            traceIfFalse "missed deadline"                 (to (pConfessionDeadline prison) `contains` txInfoValidRange info)         &&
            traceIfFalse "wrong fine"                      (lovelaces (txOutValue ownInput) == (2 * pFine prison))                    &&
            traceIfFalse "Prison NFT must go to constable" nftToConstable

        (PrisonDatum _ Nothing, PostBailOne)  ->  
            traceIfFalse "not signed by first prisoner"    (txSignedBy info (unPaymentPubKeyHash (pOne prison)))                      &&
            traceIfFalse "too early"                       (from (1 + pConfessionDeadline prison) `contains` txInfoValidRange info)   && 
            traceIfFalse "first's prisoner's fine missing" (lovelaces (txOutValue ownInput) == pFine prison)                          &&
            traceIfFalse "Prison NFT must go to constable" nftToConstable
        
        (PrisonDatum _ Nothing, PostBailTwo)  -> 
            traceIfFalse "not signed by second prisoner"   (txSignedBy info (unPaymentPubKeyHash (pTwo prison)))                      &&
            traceIfFalse "too early"                       (from (1 + pConfessionDeadline prison) `contains` txInfoValidRange info)   &&
            traceIfFalse "wrong fine"                      (lovelaces (txOutValue ownInput) == (2 * pFine prison))                    && 
            traceIfFalse "Prison NFT must go to constable" nftToConstable

        _                                        -> False 

    
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx  

        ownInput :: TxOut
        ownInput =  case findOwnInput ctx of 
                        Nothing -> traceError "prison input missing"
                        Just i -> txInInfoResolved i 

        ownOutput :: TxOut 
        ownOutput = case getContinuingOutputs ctx of 
            [o] -> o 
            _   -> traceError "exoected only one game output" 

        outputDatumhash :: DatumHash 
        outputDatumhash = case txOutDatumHash ownOutput of 
                                Nothing -> traceError "datumHash missing"
                                Just dh -> dh

        -- outputDatum' :: PrisonDatum
        -- outputDatum' = case findDatum outputDatumhash info of 
        --                     Nothing -> traceError "prison output datum not found"
        --                     Just d  -> case prisonDatum'' d of 
        --                                     Nothing -> traceError "couldn't convert datum to PrisonDatum"
        --                                     Just d -> d

        outputDatum :: PrisonDatum 
        outputDatum = case prisonDatum' (findDatum outputDatumhash info) of
                            Nothing -> traceError "prison output datum not found"
                            Just d -> d 

        checkNonce :: BuiltinByteString -> BuiltinByteString -> PrisonerChoice -> Bool 
        checkNonce bs nonce cSecond = sha2_256 (nonce `appendByteString` cFirst) == bs 
            where 
                cFirst :: BuiltinByteString
                cFirst = case cSecond of 
                    Defect    -> "bsDefect"
                    Cooperate -> "bsCooperate"

        nftToConstable :: Bool 
        nftToConstable = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash (pConstable prison)) (pToken prison) == 1


data PrisonGaming
instance Scripts.ValidatorTypes PrisonGaming where 
    type instance DatumType PrisonGaming = PrisonDatum 
    type instance RedeemerType PrisonGaming = PrisonRedeemer

bsDefect, bsCooperate :: BuiltinByteString
bsDefect    = "defect"
bsCooperate = "coop"

typedPrisonValidator :: Prison -> Scripts.TypedValidator PrisonGaming 
typedPrisonValidator prison = Scripts.mkTypedValidator @PrisonGaming
        ($$(PlutusTx.compile [|| mkPrisonValidator||])
            `PlutusTx.applyCode` PlutusTx.liftCode prison
            `PlutusTx.applyCode` PlutusTx.liftCode bsDefect
            `PlutusTx.applyCode` PlutusTx.liftCode bsCooperate)
        $$(PlutusTx.compile [|| wrap||])
    where 
        wrap = Scripts.wrapValidator @PrisonDatum @PrisonRedeemer

prisonValidator :: Prison -> Validator 
prisonValidator = Scripts.validatorScript . typedPrisonValidator

prisonAddress :: Prison -> Ledger.Address
prisonAddress = scriptAddress . prisonValidator

findPrisonOutput :: Prison -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, PrisonDatum))
findPrisonOutput prison = do 
        utxos <- utxosAt $ prisonAddress prison 
        return $ do 
            (oref, o) <- find f $ Map.toList utxos
            Datum dat <- case _ciTxOutDatum o of 
                                Left _  -> Nothing
                                Right d -> Just d
            pDat <- PlutusTx.fromBuiltinData dat 
            return (oref, o, pDat)
    where 
        f :: (TxOutRef ,ChainIndexTxOut) -> Bool 
        f ( _ , o) = assetClassValueOf (txOutValue $ toTxOut o) (pToken prison) == 1

waitUnitlTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUnitlTimeHasPassed t = do 
    s1 <- currentSlot 
    logInfo @String $ "current slot: " ++ show s1 ++ ", waiting until" ++ show t
    void $ awaitTime t >> waitNSlots 1
    s2 <- currentSlot
    logInfo @String $ "waited until: " ++ show s2 


data CreatePrisonParams = CreatePrisonParams
    { cpPrisonerOne          :: !PaymentPubKeyHash
    , cpPrisonerTwo          :: !PaymentPubKeyHash
    , cpFine                 :: !Integer
    , cpPrisonerOnePenalty   :: !(Integer, Integer)
    , cpPrisonerTwoPenalty   :: !(Integer, Integer)
    , cpConfessionDeadline   :: !POSIXTime 
    , cpSentencingDeadline   :: !POSIXTime
    , cpCurrency             :: !CurrencySymbol
    , cpTokenName            :: !TokenName
    }
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

createPrison :: forall w s. CreatePrisonParams -> Contract w s Text ()
createPrison cp = do 
    pkh <- Contract.ownPaymentPubKeyHash
    let prison = Prison 
            { pOne                = cpPrisonerOne cp
            , pTwo                = cpPrisonerTwo cp
            , pConstable          = pkh
            , pFine               = cpFine cp
            , pOnePenalty         = cpPrisonerOnePenalty cp
            , pTwoPenalty         = cpPrisonerTwoPenalty cp
            , pConfessionDeadline = cpConfessionDeadline cp
            , pSentencingDeadline = cpSentencingDeadline cp
            , pToken              = AssetClass ( cpCurrency cp, cpTokenName cp)
            }
        v  = lovelaceValueOf 0 <> assetClassValue (pToken prison) 1
        bs = emptyByteString
        tx = Constraints.mustPayToTheScript (PrisonDatum bs Nothing) v
    ledgerTx <- submitTxConstraints (typedPrisonValidator prison) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "created prison with fine: " ++ show (cpFine cp)

data FirstParams = FirstParams 
    { fpSecond              :: !PaymentPubKeyHash
    , fpConstable           :: !PaymentPubKeyHash
    , fpFine                :: !Integer
    , fpPrisonerOnePenalty  :: !(Integer, Integer)
    , fpPrisonerTwoPenalty  :: !(Integer, Integer)
    , fpConfessionDeadline  :: !POSIXTime
    , fpSentencingDeadline  :: !POSIXTime
    , fpNonce               :: !BuiltinByteString
    , fpCurrency            :: !CurrencySymbol
    , fpTokenName           :: !TokenName
    , fpChoice              :: !PrisonerChoice
    } 
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
    -- deriving anyclass (OpenApi.ToSchema)

firstPrison :: forall w s. FirstParams -> Contract w s Text ()
firstPrison fp = do 
    pkh <- Contract.ownPaymentPubKeyHash
    let prison = Prison 
            { pOne                = pkh
            , pTwo                = fpSecond fp
            , pConstable          = fpConstable fp
            , pFine               = fpFine fp
            , pOnePenalty         = fpPrisonerOnePenalty fp
            , pTwoPenalty         = fpPrisonerTwoPenalty fp
            , pConfessionDeadline = fpConfessionDeadline fp
            , pSentencingDeadline = fpSentencingDeadline fp
            , pToken              = AssetClass (fpCurrency fp, fpTokenName fp) 
            }

    m <- findPrisonOutput prison
    case m of 
        Nothing          -> throwError "prison output not found!!"
        Just (oref, o, dat) -> case dat of 
            PrisonDatum emptyByteString Nothing -> do 
                logInfo @String "constable has created the prison"
                now <- currentTime
                let token   = assetClassValue (pToken prison) 1
                let v       = let x = lovelaceValueOf (fpFine fp) in x <> x <> token
                    c       = fpChoice fp 
                    bs      = sha2_256 $ fpNonce fp `appendByteString` if c == Defect then bsDefect else bsCooperate
                    lookups = Constraints.unspentOutputs (Map.singleton oref o)                 <>
                              Constraints.otherScript (prisonValidator prison)                  <>
                              Constraints.typedValidatorLookups (typedPrisonValidator prison)
                    tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Action c)  <>
                              Constraints.mustPayToTheScript (PrisonDatum bs $ Just c) v                             <>
                              Constraints.mustValidateIn (to now)
                ledgerTx <- submitTxConstraintsWith @PrisonGaming lookups tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ "Prisoner one made confession: " ++ show (fpChoice fp)

                waitUnitlTimeHasPassed $ fpConfessionDeadline fp

                m   <- findPrisonOutput prison 
                now <- currentTime 
                case m of 
                    Nothing           -> throwError "prison output not found"
                    Just (oref, o, dat) -> case dat of
                        PrisonDatum _ Nothing -> do 
                            logInfo @String  "second prisoner did not confess"
                            let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                                        Constraints.otherScript (prisonValidator prison)
                                tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData PostBailOne) <>
                                        Constraints.mustValidateIn (to $ now + 1000)
                            ledgerTx' <- submitTxConstraintsWith @PrisonGaming lookups tx'
                            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                            logInfo @String "reclaim fine"

                        PrisonDatum _ (Just c') | c' == c -> do 
                            logInfo @String "second player played and picked same"
                            let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                                        Constraints.otherScript (prisonValidator prison)
                                tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData PostBailTwo) <>
                                        Constraints.mustValidateIn (to $ now + 1000)
                            ledgerTx' <- submitTxConstraintsWith @PrisonGaming lookups tx'
                            void $  awaitTxConfirmed $ getCardanoTxId ledgerTx'
                            logInfo @String "Moving to sentencing"

                        _ -> logInfo @String "second prisoner has confessed"

            --  _ -> logInfo @String "no running prison found"

    -- waitUnitlTimeHasPassed $ fpConfessionDeadline fp

    -- m   <- findPrisonOutput prison 
    -- now <- currentTime 
    -- case m of 
    --     Nothing           -> throwError "prison output not found"
    --     Just (oref, o, dat) -> case dat of
    --         PrisonDatum _ Nothing -> do 
    --             logInfo @String  "second prisoner did not confess"
    --             let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
    --                           Constraints.otherScript (prisonValidator prison)
    --                 tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData PostBailOne) <>
    --                           Constraints.mustValidateIn (to $ now + 1000)
    --             ledgerTx' <- submitTxConstraintsWith @PrisonGaming lookups tx'
    --             void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
    --             logInfo @String "reclaim fine"

    --         PrisonDatum _ (Just c') | c' == c -> do 
    --             logInfo @String "second player played and picked same"
    --             let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
    --                           Constraints.otherScript (prisonValidator prison)
    --                 tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData PostBailTwo) <>
    --                           Constraints.mustValidateIn (to $ now + 1000)
    --             ledgerTx' <- submitTxConstraintsWith @PrisonGaming lookups tx'
    --             void $  awaitTxConfirmed $ getCardanoTxId ledgerTx'
    --             logInfo @String "Moving to sentencing"

    --         _ -> logInfo @String "second prisoner has confessed"

data SecondParams = SecondParams 
    { spFirst               :: !PaymentPubKeyHash
    , spConstable           :: !PaymentPubKeyHash
    , spFine                :: !Integer
    , spPrisonerOnePenalty  :: !(Integer, Integer)
    , spPrisonerTwoPenalty  :: !(Integer, Integer)
    , spConfessionDeadline  :: !POSIXTime
    , spSentencingDeadline  :: !POSIXTime
    , spCurrency            :: !CurrencySymbol
    , spTokenName           :: !TokenName
    , spChoice              :: !PrisonerChoice
    } 
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

secondPrison :: forall w s. SecondParams -> Contract w s Text ()
secondPrison sp = do 
    pkh <- Contract.ownPaymentPubKeyHash
    let prison = Prison 
            { pOne                = spFirst sp 
            , pTwo                = pkh
            , pConstable          = spConstable sp 
            , pFine               = spFine sp
            , pOnePenalty         = spPrisonerOnePenalty sp
            , pTwoPenalty         = spPrisonerTwoPenalty sp
            , pConfessionDeadline = spConfessionDeadline sp 
            , pSentencingDeadline = spSentencingDeadline sp 
            , pToken              = AssetClass (spCurrency sp, spTokenName sp)
            }
    m <- findPrisonOutput prison
    case m of 
        Just (oref, o, PrisonDatum bs Nothing) -> do 
            logInfo @String "runnning prison found"
            now <- currentTime
            let token   = assetClassValue (pToken prison) 1
            let v       = let x = lovelaceValueOf (spFine sp) in x <> x <> token
                c       = spChoice sp
                lookups = Constraints.unspentOutputs (Map.singleton oref o)                                     <>
                          Constraints.otherScript (prisonValidator prison)                                      <>
                          Constraints.typedValidatorLookups (typedPrisonValidator prison)
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Action c) <>
                          Constraints.mustPayToTheScript (PrisonDatum bs $ Just c) v                            <>
                          Constraints.mustValidateIn (to now)
            ledgerTx <- submitTxConstraintsWith @PrisonGaming lookups tx
            let tid = getCardanoTxId ledgerTx
            void $ awaitTxConfirmed tid
            logInfo @String $ "made second move: " ++ show (spChoice sp)

            waitUnitlTimeHasPassed $ spConfessionDeadline sp 

            m'   <- findPrisonOutput prison 
            now' <- currentTime 
            case m' of 
                Nothing               -> logInfo @String "first player confessed"
                Just (oref', o', _)   -> do
                    logInfo @String "first player didn't show confession"
                    let lookups'    = Constraints.unspentOutputs (Map.singleton oref' o')                                     <>
                                      Constraints.otherScript (prisonValidator prison)
                        tx'         = Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData PostBailTwo) <>
                                      Constraints.mustValidateIn (from now')                                                  <>
                                      Constraints.mustPayToPubKey (spConstable sp) token
                    ledgerTx' <- submitTxConstraintsWith @PrisonGaming lookups' tx'
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                    logInfo @String "second prisoner posted bail"

        _ -> logInfo @String "no running prison found"

type PrisonSchema = Endpoint "create" CreatePrisonParams 
                    .\/ Endpoint "first" FirstParams 
                    .\/ Endpoint "second" SecondParams

endpoints :: Contract () PrisonSchema Text ()
endpoints = awaitPromise $ (create `select` first `select` second)
    where 
        create = endpoint @"create" $ \cp -> do 
            createPrison cp 

        first = endpoint @"first" $ \fp -> do 
            firstPrison fp

        second = endpoint @"second" $ \sp -> do 
            secondPrison sp 


























-- transition :: Prison -> State PrisonDatum -> PrisonRedeemer -> Maybe (TxConstraints Void Void, State PrisonDatum)
-- transition prison s r = case (stateValue s, stateData s, r) of 
--     (v, PrisonDatum bs Nothing, Action c)
--         |lovelaces v == pFine prison                  -> Just ( Constraints.mustBeSignedBy (pTwo prison)                         <>
--                                                                 Constraints.mustValidateIn (to $ pConfessionDeadline prison)
--                                                                ,State (PrisonDatum bs $ Just c) (lovelaceValueOf $ 2 * pFine prison) 
--                                                               )
--     (v, PrisonDatum _ (Just c), Reveal _ c')
--         | lovelaces v == pFine prison                 -> Just ( Constraints.mustBeSignedBy (pOne prison)                         <>
--                                                                 Constraints.mustValidateIn (to $ pSentencingDeadline prison)     <>
--                                                                 Constraints.mustPayToPubKey (pConstable prison)
--                                                                                             (lovelaceValueOf $ 10 * pFine prison)
--                                                               , State Finished mempty
--                                                               )
--     (v, PrisonDatum bs (Just c), Reveal _ c')
--         case (sentence c c') of
--                 (10,5)                                -> Just ( Constraints.mustBeSignedBy (pOne prison)                         <>
--                                                                 Constraints.mustValidateIn (to $ pSentencingDeadline prison)     <>
--                                                                 Constraints.mustPayToPubKey (pConstable prison)
--                                                                                             (lovelaceValueOf $ 10 * pFine prison)
--                                                               , State Finished mempty
--                                                               )
--         -- |case (sentence c c') of 
--         --         (10, 5)                               -> Just ( Constraints.mustBeSignedBy (pOne prison)                        <>
--         --                                                         Constraints.mustValidateIn (to $ pSentencingDeadline prison)    <>
--         --                                                         Constraints.mustPayToPubKey (pConstable prison) 
--         --                                                                                     (lovelaceValueOf $ 10 * pFine prison)
--         --                                                       , State Finished mempty
--         --                                                       )
--         --         -- (5, 10)                               -> Just ( Constraints.mustBeSignedBy (pOne prison)                        <>
--                 --                                                 Constraints.mustValidateIn (to $ pSentencingDeadline prison)    <>
--                 --                                                 Constraints.mustPayToPubKey (pConstable prison) 
--                 --                                                                             (lovelaceValueOf $ 5 * pFine prison)
--                 --                                               , State Finished mempty
--                 --                                               )
--                 -- (3, 3)                                -> Just ( Constraints.mustBeSignedBy (pOne prison)                        <>
--                 --                                                 Constraints.mustValidateIn (to $ pSentencingDeadline prison)    <>
--                 --                                                 Constraints.mustPayToPubKey (pConstable prison) 
--                 --                                                                             (lovelaceValueOf $ 3 * pFine prison)
--                 --                                               , State Finished mempty
--                 --                                               )
--                 -- (15, 15)                              -> Just ( Constraints.mustBeSignedBy (pOne prison)                        <>
--                 --                                                 Constraints.mustValidateIn (to $ pSentencingDeadline prison)    <>
--                 --                                                 Constraints.mustPayToPubKey (pConstable prison)
--                 --                                                                             (lovelaceValueOf $ 15 * pFine prison)
--                 --                                               , State Finished mempty
--                 --                                               )
--     _                                                 -> Nothing 
                                                            


