{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module OracleSwap
    ( SwapSchema
    , swap
    ) where

import           Control.Monad        hiding (fmap)
import           Data.List            (find)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Monoid          (Last (..))
import           Data.Text            (Text)
import           Plutus.Contract      as Contract
-- import           Plutus.V1.Ledger        (PaymentPubKeyHash (unPaymentPubKeyHash))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), (<$>), unless, mapMaybe, find)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada hiding (divide)
import           Ledger.Value         as Value
import           Prelude              (Semigroup (..), Show (..), String, (<$>))

import           OracleCore
import           OracleFunds

{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =
    txSignedBy info pkh ||
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
     traceIfFalse "price not paid"                     sellerPaid)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleInput :: TxOut
    oracleInput =
      let
        ins = [ o
              | i <- txInfoInputs info
              , let o = txInInfoResolved i
              , txOutAddress o == addr
              ]
      in
        case ins of
            [o] -> o
            _   -> traceError "expected exactly one oracle input"

    oracleValue' = case oracleValue oracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x

    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
      in
        length xs == 2

    minPrice :: Integer
    minPrice =
      let
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
      in
        price lovelaceIn oracleValue'

    sellerPaid :: Bool
    sellerPaid =
      let
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
      in
        pricePaid >= minPrice

data Swapping
instance Scripts.ValidatorTypes Swapping where
    type instance DatumType Swapping = PubKeyHash
    type instance RedeemerType Swapping = ()

typedSwapValidator :: Oracle -> Scripts.TypedValidator Swapping
typedSwapValidator oracle = Scripts.mkTypedValidator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()

swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . typedSwapValidator

swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator

offerSwap :: forall w s. Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do
    pkh <- Contract.ownPaymentPubKeyHash
    let ppkh = unPaymentPubKeyHash pkh
        tx   = Constraints.mustPayToTheScript ppkh $ Ada.lovelaceValueOf amt
    ledgerTx <- submitTxConstraints (typedSwapValidator oracle) tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"

findSwaps :: Oracle -> (PaymentPubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, ChainIndexTxOut, PaymentPubKeyHash)]
findSwaps oracle p = do
    utxos <- utxosAt $ swapAddress oracle
    return $ mapMaybe g $ Map.toList utxos
  where
    f :: ChainIndexTxOut -> Maybe PaymentPubKeyHash
    f o = do
        dh        <- case _ciTxOutDatum o of 
                                        Left dh -> Just dh 
                                        Right _ -> Nothing
        (Datum d) <- case _ciTxOutDatum o of 
                                Left _ -> Nothing
                                Right d -> Just d
        PlutusTx.fromBuiltinData d

    g :: (TxOutRef, ChainIndexTxOut) -> Maybe (TxOutRef, ChainIndexTxOut, PaymentPubKeyHash)
    g (oref, o) = do
        pkh <- f o
        guard $ p pkh
        return (oref, o, pkh)

retrieveSwaps :: Oracle -> Contract w s Text ()
retrieveSwaps oracle = do
    pkh <- Contract.ownPaymentPubKeyHash
    -- let pkh = unPaymentPubKeyHash ppkh
    xs  <- findSwaps oracle (== pkh)
    case xs of
        [] -> logInfo @String "no swaps found"
        _  -> do
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                          Constraints.otherScript (swapValidator oracle)
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | (oref, _, _) <- xs]
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"

useSwap :: forall w s. Oracle -> Contract w s Text ()
useSwap oracle = do
    funds <- ownFunds
    let amt = assetClassValueOf funds $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle
    case m of
        Nothing           -> logInfo @String "oracle not found"
        Just (oref, o, x) -> do
            logInfo @String $ "found oracle, exchange rate " ++ show x
            pkh   <- Contract.ownPaymentPubKeyHash
            swaps <- findSwaps oracle (/= pkh)
            case find (f amt x) swaps of
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do
                    -- let ppkh'    = unPaymentPubKeyHash ppkh'
                    let v       = _ciTxOutValue o <> lovelaceValueOf (oFee oracle)
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ _ciTxOutValue o') x
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                  Constraints.otherScript (oracleValidator oracle)                   <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toBuiltinData Use) <>
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ())  <>
                                  Constraints.mustPayToOtherScript
                                    (validatorHash $ oracleValidator oracle)
                                    (Datum $ PlutusTx.toBuiltinData x)
                                    v                                                                             <>
                                  Constraints.mustPayToPubKey pkh' p
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> ChainIndexTxOut -> Integer
    getPrice x o = price (lovelaces $ _ciTxOutValue o) x

    f :: Integer -> Integer -> (TxOutRef, ChainIndexTxOut, PaymentPubKeyHash) -> Bool
    f amt x (_, o, _) = getPrice x o <= amt

type SwapSchema =
            Endpoint "offer"    Integer
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      ()
        .\/ Endpoint "funds"    ()

swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
swap oracle = awaitPromise (offer `select` retrieve `select` use `select` funds) >> swap oracle
  where
    offer :: Promise (Last Value) SwapSchema Text ()
    offer = endpoint @"offer" $ \(amt) -> do 
              offerSwap oracle amt

    retrieve :: Promise (Last Value) SwapSchema Text ()
    retrieve = endpoint @"retrieve" $ \_ -> do 
                retrieveSwaps oracle

    use :: Promise (Last Value) SwapSchema Text ()
    use = endpoint @"use" $ \ _ -> do 
            useSwap oracle

    funds :: Promise (Last Value) SwapSchema Text ()
    funds = endpoint @"funds" $ \ _ -> do
              v <- ownFunds
              tell $ Last $ Just v

    h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
    h = handleError logError
