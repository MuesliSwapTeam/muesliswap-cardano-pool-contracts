{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module MuesliSwapPools.ConstantProductFactory.Types
  ( FactoryParams (..),
    FactoryRedeemer (..),
  )
where

import qualified Plutus.V2.Ledger.Api as V2
import qualified PlutusTx
import qualified Prelude as Haskell

data FactoryParams = FactoryParams
  { fpNftSymbol :: V2.CurrencySymbol,
    fpLiquiditySymbol :: V2.CurrencySymbol,
    fpFactoryTokenName :: V2.TokenName,
    fpCreatorLicenseSymbol :: V2.CurrencySymbol
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''FactoryParams [('FactoryParams, 0)]
PlutusTx.makeLift ''FactoryParams

data FactoryRedeemer = CreatePool
  { apLicenseIndex :: Haskell.Integer
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''FactoryRedeemer [('CreatePool, 0)]