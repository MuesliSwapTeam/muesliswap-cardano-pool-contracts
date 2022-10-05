{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Api hiding (Script)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), fromPlutusData)
import Codec.Serialise (serialise)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Ledger (Script, getTxId)
import MuesliSwapPools.BatchOrder.OnChain (mkBatchOrderScript)
import MuesliSwapPools.ConstantProductFactory.OnChain (mkFactoryScript)
import MuesliSwapPools.ConstantProductLiquidity.OnChain (mkLiquidityScript)
import MuesliSwapPools.ConstantProductPool.OnChain (mkPoolScript)
import MuesliSwapPools.ConstantProductPoolNFT.OnChain (mkNFTScript)
import qualified Plutus.V2.Ledger.Api as V2
import PlutusTx.Prelude (sha2_256)
import MuesliSwapPools.Utils.OnChainUtils (integerToBS)
import qualified PlutusTx
import Data.Aeson (encode)
import Prelude

main :: IO ()
main = do
  writePlutusScript' "NFT minting" "plutus/nft_minting_policy.plutus" mkNFTScript
  writePlutusScript' "LP minting" "plutus/lp_minting_policy.plutus" mkLiquidityScript
  writePlutusScript' "Factory minting" "plutus/factory_minting_policy.plutus" mkFactoryScript
  writePlutusScript' "Pool" "plutus/pool_script.plutus" mkPoolScript
  writePlutusScript' "Batch order" "plutus/batch_order_script.plutus" mkBatchOrderScript

writePlutusScript' :: String -> FilePath -> Script -> IO ()
writePlutusScript' title filename scrpt =
  do
    let scriptSBS = SBS.toShort . LBS.toStrict . serialise $ scrpt
    let scriptSerial = PlutusScriptSerialised scriptSBS :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope filename Nothing scriptSerial
    case result of
      Left err -> print $ displayError err
      Right () -> return ()