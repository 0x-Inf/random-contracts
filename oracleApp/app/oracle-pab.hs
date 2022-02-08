{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void, when)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, Result (..), fromJSON)
import           Data.Default                        (Default (..))
import           Data.Monoid                         (Last (..))
import           Data.Text                           (Text, pack)
import           Data.Void                           (Void)
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Constraints                  as Constraints  
import qualified Ledger.Value                        as Value
import           Plutus.Contract                     as Contract
import           Plutus.Contract
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), HasDefinitions (..) , BuiltinHandler (..), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Plutus.Contracts.Currency           as Currency

import           Wallet.Emulator.Types               (Wallet (..), knownWallet, knownWallets, mockWalletPaymentPubKeyHash)
import           Wallet.Types                        (ContractInstanceId (..))

import qualified OracleCore                          as Oracle
import           OraclePAB                           (OracleContracts (..))
import qualified OracleSwap                          as Oracle

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin OracleContracts) "Starting Oracle PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit <- Simulator.activateContract (knownWallet 1) Init
    cs      <- waitForLast cidInit
    _       <- Simulator.waitUntilFinished cidInit

    cidOracle <- Simulator.activateContract (knownWallet 1) $ Oracle cs
    liftIO $ writeFile "oracle.cid" $ show $ unContractInstanceId cidOracle
    oracle <- waitForLast cidOracle

    forM_ wallets $ \w ->
        when (w /= knownWallet 1) $ do
            cid <- Simulator.activateContract w $ Swap oracle
            liftIO $ writeFile ('W' : show (getWalletId w) ++ ".cid") $ show $ unContractInstanceId cid

    void $ liftIO getLine
    shutdown

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing

wallets :: [Wallet]
wallets = take 5 knownWallets

usdt :: TokenName
usdt = "USDT"

oracleParams :: CurrencySymbol -> Oracle.OracleParams
oracleParams cs = Oracle.OracleParams
    { Oracle.opFees   = 1_000_000
    , Oracle.opSymbol = cs
    , Oracle.opToken  = usdt
    }

-- handleOracleContracts ::
--     ( Member (Error PABError) effs
--     , Member (LogMsg (PABMultiAgentMsg (Builtin OracleContracts))) effs
--     )
--     => ContractEffect (Builtin OracleContracts)
--     ~> Eff effs
-- handleOracleContracts = handleBuiltin @OracleContracts

instance HasDefinitions OracleContracts where
    -- getDefinitions = [Init, Swap, Oracle]
    getSchema = \case
        Init     -> endpointsToSchemas @Empty
        Oracle _ -> endpointsToSchemas @Oracle.OracleSchema
        Swap _   -> endpointsToSchemas @Oracle.SwapSchema
    getContract = \case
        Init        -> SomeBuiltin   initContract
        Oracle cs   -> SomeBuiltin $ Oracle.runOracle $ oracleParams cs
        Swap oracle -> SomeBuiltin $ Oracle.swap oracle

handlers :: SimulatorEffectHandlers (Builtin OracleContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin OracleContracts) def def
    $ interpret  (contractHandler (handleBuiltin @OracleContracts))

initContract :: Contract (Last CurrencySymbol) Empty Text ()
initContract = do
    ownPK <- Contract.ownPaymentPubKeyHash
    cur   <-
        mapError (pack . show)
        (Currency.mintContract ownPK [(usdt, fromIntegral (length wallets) * amount)]
        :: Contract (Last CurrencySymbol) Empty Currency.CurrencyError Currency.OneShotCurrency)
    let cs = Currency.currencySymbol cur
        v  = Value.singleton cs usdt amount
    forM_ wallets $ \w -> do
        let pkh = mockWalletPaymentPubKeyHash w
        when (pkh /= ownPK) $ do
            mkTxConstraints @Void mempty (Constraints.mustPayToPubKey pkh v)
                >>= submitTxConfirmed . Constraints.adjustUnbalancedTx
            -- tx <- submitTx $ mustPayToPubKey pkh v
            -- awaitTxConfirmed $ getCardanoTxId tx
    tell $ Last $ Just cs
  where
    amount :: Integer
    amount = 100_000_000
