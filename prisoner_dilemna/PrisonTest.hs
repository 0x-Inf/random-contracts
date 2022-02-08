{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module PrisonTest where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Ledger
import           Ledger.TimeSlot
import           Ledger.Value
import           Ledger.Ada                 as Ada
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..))
import           Wallet.Emulator.Wallet

import           Prison 

test :: IO ()
test = do 
    test' Cooperate Defect 
    -- test' Cooperate Cooperate 
    -- test' Defect Cooperate
    -- test' Defect Defect 

test' :: PrisonerChoice -> PrisonerChoice -> IO ()
test' c1 c2 = runEmulatorTraceIO' def emCfg $ myTrace c1 c2
    where 
        emCfg :: EmulatorConfig
        emCfg = EmulatorConfig (Left $ Map.fromList
            [ (knownWallet 1, v <> assetClassValue (AssetClass (prisonTokenCurrency, prisonTokenName)) 1 )
            , (knownWallet 2, v)
            , (knownWallet 3, v)
            ]) def def 
        
        v :: Value 
        v = Ada.lovelaceValueOf 1_000_000_000

prisonTokenCurrency :: CurrencySymbol
prisonTokenCurrency = "ff"

prisonTokenName :: TokenName
prisonTokenName = "PRISON TOKEN"

myTrace :: PrisonerChoice -> PrisonerChoice -> EmulatorTrace ()
myTrace c1 c2 = do 
    Extras.logInfo $ "first action: " ++ show c1 ++ ", second action: " ++ show c2 

    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    h3 <- activateContractWallet (knownWallet 3) endpoints 

    let pkh1      = mockWalletPaymentPubKeyHash (knownWallet 1)
        pkh2      = mockWalletPaymentPubKeyHash (knownWallet 2)
        pkh3      = mockWalletPaymentPubKeyHash (knownWallet 3)
        fine      = 5_000_000
        deadline1 = slotToBeginPOSIXTime def 10 
        deadline2 = slotToBeginPOSIXTime def 10 

        cp = CreatePrisonParams
                { cpPrisonerOne        = pkh2 
                , cpPrisonerTwo        = pkh3
                , cpFine               = fine
                , cpPrisonerOnePenalty = (15,5)
                , cpPrisonerTwoPenalty = (15,5)
                , cpConfessionDeadline = deadline1
                , cpSentencingDeadline = deadline2
                , cpCurrency           = prisonTokenCurrency
                , cpTokenName          = prisonTokenName
                }

        fp = FirstParams
                { fpSecond             = pkh3
                , fpConstable          = pkh1 
                , fpFine               = fine
                , fpPrisonerOnePenalty = (15,5)
                , fpPrisonerTwoPenalty = (15,5)
                , fpConfessionDeadline = deadline1
                , fpSentencingDeadline = deadline2
                , fpNonce              = "SECRETNONCE"
                , fpCurrency           = prisonTokenCurrency
                , fpTokenName          = prisonTokenName
                , fpChoice             = c1
                }
        
        sp = SecondParams 
                { spFirst              = pkh2
                , spConstable          = pkh1
                , spFine               = fine
                , spPrisonerOnePenalty = (15,5)
                , spPrisonerTwoPenalty = (15,5)
                , spConfessionDeadline = deadline1
                , spSentencingDeadline = deadline2
                , spCurrency           = prisonTokenCurrency
                , spTokenName          = prisonTokenName
                , spChoice             = c2
                }
    callEndpoint @"create" h1 cp

    void $ Emulator.waitNSlots 3

    callEndpoint @"first" h2 fp 

    void $ Emulator.waitNSlots 5 

    callEndpoint @"second" h3 sp 
    
    void $ Emulator.waitNSlots 15 