{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, Result (..), ToJSON, encode, fromJSON)
import qualified Data.ByteString.Lazy                as LB
import           Data.Default                        (Default (def))
import qualified Data.Monoid                         as Monoid
import qualified Data.Semigroup                      as Semigroup
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Prettyprinter                       (Pretty (..), viaShow  )
import           Plutus.Contract             
import qualified Plutus.Contracts.Currency           as Currency
import qualified Plutus.Contracts.Uniswap            as Uniswap
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers, logString)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Prelude                             hiding (init)
import           Wallet.Emulator.Types               (Wallet (..), knownWallet)
import           Wallet.Types                        (ContractInstanceId (..))
import           Plutus.Contracts.Uniswap.Trace      as US
import           Uniswap                             as USS

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    logString @(Builtin UniswapContracts) "Starting Uniswap PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit  <- Simulator.activateContract (knownWallet 1) Init
    cs       <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
                    Success (Just (Semigroup.Last cur)) -> Just $ Currency.currencySymbol cur
                    _                                   -> Nothing
    _        <- Simulator.waitUntilFinished cidInit

    liftIO $ LB.writeFile "symbol.json" $ encode cs
    logString @(Builtin UniswapContracts) $ "Initialization finished. Minted: " ++ show cs

    cidStart <- Simulator.activateContract (knownWallet 1) UniswapStart
    us       <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.Uniswap))) of
                    Success (Monoid.Last (Just (Right us))) -> Just us
                    _                                       -> Nothing
    logString @(Builtin UniswapContracts) $ "Uniswap instance created: " ++ show us

    forM_ USS.wallets $ \w -> do
        cid <- Simulator.activateContract w $ UniswapUser us
        liftIO $ writeFile (cidFile w) $ show $ unContractInstanceId cid
        logString @(Builtin UniswapContracts) $ "Uniswap user contract started for " ++ show w

    void $ liftIO getLine

    shutdown

-- data UniswapContracts =
--       Init
--     | UniswapStart
--     | UniswapUser Uniswap.Uniswap
--     deriving (Eq, Ord, Show, Generic)
--     deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)
--     -- deriving instance ToJSON

-- instance Pretty UniswapContracts where
--     pretty = viaShow

-- instance HasDefinitions UniswapContracts where
--     getDefinitions = [Init, UniswapStart]
--     getSchema = \case
--         UniswapUser _ -> Builtin.endpointsToSchemas @Uniswap.UniswapUserSchema
--         UniswapStart  -> Builtin.endpointsToSchemas @Uniswap.UniswapOwnerSchema
--         Init          -> Builtin.endpointsToSchemas @Empty
--     getContract = \case
--         UniswapUser us -> SomeBuiltin . awaitPromise $ Uniswap.userEndpoints us
--         UniswapStart   -> SomeBuiltin Uniswap.ownerEndpoint
--         Init           -> SomeBuiltin US.setupTokens

handlers :: SimulatorEffectHandlers (Builtin UniswapContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @UniswapContracts))
