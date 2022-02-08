{-# LANGUAGE DataKinds             #-}
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

module OracleFunds
    ( ownFunds
    , ownFunds'
    ) where

import           Control.Monad    hiding (fmap)
import qualified Data.Map         as Map
import qualified Data.List        as List 
import           Data.Monoid      (Last (..))
import           Data.Text        (Text)
import           Plutus.Contract  as Contract
import           PlutusTx.Prelude hiding ((<$>))
import           Prelude          (Show (..), String, (<$>))
import           Ledger           hiding (singleton)
import           Ledger.Value     as Value

ownFunds :: Contract w s Text Value
ownFunds = do
    pk    <- ownPaymentPubKeyHash 
    utxos <- utxosAt $ pubKeyHashAddress pk Nothing 
    let v =   mconcat $ List.map _ciTxOutValue $ Map.elems utxos
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
    return v

ownFunds' :: Contract (Last Value) Empty Text ()
ownFunds' = do
    handleError logError $ ownFunds >>= tell . Last . Just
    void $ Contract.waitNSlots 1
    ownFunds'
