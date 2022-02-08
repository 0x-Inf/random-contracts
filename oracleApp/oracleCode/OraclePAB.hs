{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module OraclePAB
    ( OracleContracts (..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           GHC.Generics              (Generic)
import qualified Data.OpenApi.Schema       as OpenApi
import           Ledger

import qualified OracleCore       as Oracle

data OracleContracts = Init | Oracle CurrencySymbol | Swap Oracle.Oracle
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
    deriving anyclass (OpenApi.ToSchema)

instance Pretty OracleContracts where
    pretty = viaShow

-- instance HasDefinitions OracleContracts where
--     getDefinitions = [Init, Oracle, Swap]
--     getSchema = \case
--         Init     -> endpointsToSchemas @Empty
--         Oracle _ -> endpointsToSchemas @Oracle.OracleSchema
--         Swap _   -> endpointsToSchemas @Oracle.SwapSchema
--     getContract = \case
--         Init        -> SomeBuiltin   initContract
--         Oracle cs   -> SomeBuiltin $ Oracle.runOracle $ oracleParams cs
--         Swap oracle -> SomeBuiltin $ Oracle.swap oracle

