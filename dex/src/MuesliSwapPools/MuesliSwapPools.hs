{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Api hiding (Script)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Maybe (fromJust)
import Ledger (Script, getTxId, scriptCurrencySymbol)
import MuesliSwapPools.BatchOrder.OnChain
import MuesliSwapPools.ConstantProductFactory.OnChain
import MuesliSwapPools.ConstantProductFactory.Types (FactoryParams (..), FactoryRedeemer (apLicenseIndex))
import MuesliSwapPools.ConstantProductLiquidity.OnChain (mkLiquiditySymbol, mkLiquidityScript)
import MuesliSwapPools.ConstantProductPool.OnChain
import MuesliSwapPools.ConstantProductPool.Types (PoolRedeemer (ApplyPool, apBatcherAddress), ProfitSharing (ProfitSharing, psFeeTo, psFeeToDatumHash))
import MuesliSwapPools.ConstantProductPool.Utils (calculateInitialLiquidity, minimumLiquidity)
import MuesliSwapPools.ConstantProductPoolNFT.OnChain (mkNFTSymbol, mkNFTScript)
import MuesliSwapPools.ConstantProductPoolNFT.Utils (poolNFTOf)
import qualified MuesliSwapPools.Spooky.TypedSpookyContexts as SC
import MuesliSwapPools.Types.Coin (adaCoin, assetClass, assetClassValueOf, tokenNameOf)
import MuesliSwapPools.Utils.OnChainUtils (assertPoolValue, integerToBS, mustFindScriptDatum')
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential), CurrencySymbol, MintingPolicy, unMintingPolicyScript)
import qualified Plutus.V1.Ledger.Api as Plutus
import Plutus.V1.Ledger.Scripts (Script, scriptSize)
import Plutus.V1.Ledger.Value (AssetClass, TokenName, Value, tokenName)
import qualified PlutusTx
import PlutusTx.Prelude (sha2_256)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  writePlutusScript' "NFT minting" "plutus/nft_minting_policy.plutus" mkNFTScript
  writePlutusScript' "LP minting" "plutus/lp_minting_policy.plutus" mkLiquidityScript
  writePlutusScript' "Factory minting" "plutus/factory_minting_policy.plutus" mkFactoryScript
  writePlutusScript' "Pool" "plutus/pool_script.plutus" mkPoolScript
  writePlutusScript' "Batch order" "plutus/batch_order_script.plutus" mkBatchOrderScript

writePlutusScript' :: String -> FilePath -> Ledger.Script -> IO ()
writePlutusScript' title filename scrpt =
  do
    let scriptSBS = SBS.toShort . LBS.toStrict . serialise $ scrpt
    let scriptSerial = PlutusScriptSerialised scriptSBS :: PlutusScript PlutusScriptV1
    case Plutus.defaultCostModelParams of
      Just m ->
        let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS []
         in do
              print title
              print ("Log output" :: String) >> print logout
              case e of
                Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                Right exbudget -> do
                  print ("Ex Budget" :: String) >> print exbudget
                  print ("Script size " :: String) >> print (scriptSize scrpt)
      Nothing -> error "defaultCostModelParams failed"
    result <- writeFileTextEnvelope filename Nothing scriptSerial
    case result of
      Left err -> print $ displayError err
      Right () -> return ()
