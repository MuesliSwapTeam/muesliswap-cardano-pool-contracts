{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MuesliSwapPools.Utils.OnChainUtils
  ( assertPoolValue,
    mustFindScriptDatum,
    integerToBS,
    bsToInteger,
    licenseDeadline,
    hasOutDatum
  )
where

import MuesliSwapPools.Types.Coin (adaCoin, assetClassValueOf)
import qualified Plutus.V2.Ledger.Api as V2
import Plutus.V2.Ledger.Contexts (TxInfo, TxOut, findDatum)
import Plutus.V1.Ledger.Tx (txOutDatum)
import qualified PlutusTx
import PlutusTx.IsData.Class (UnsafeFromData)
import PlutusTx.Prelude
import qualified PlutusTx.AssocMap as Map

minusAsciiCode :: Integer
minusAsciiCode = 45

zeroAsciiCode :: Integer
zeroAsciiCode = 48

{-# INLINEABLE assertPoolValue #-}
assertPoolValue :: (V2.CurrencySymbol, V2.TokenName) -> (V2.CurrencySymbol, V2.TokenName) -> (V2.CurrencySymbol, V2.TokenName) -> V2.Value -> Bool
assertPoolValue coinA coinB lpCoin v =
  let valueLength = length $ flatten' v
      hasLPCoin = assetClassValueOf v lpCoin > 0
      hasOneSideADA = coinA == adaCoin || coinB == adaCoin
   in if hasLPCoin && hasOneSideADA
        then valueLength == 5
        else
          if not hasLPCoin && hasOneSideADA
            then valueLength == 4
            else
              if hasLPCoin && not hasOneSideADA
                then valueLength == 6
                else valueLength == 5
  where
    flatten' :: V2.Value -> [(V2.CurrencySymbol, V2.TokenName, Integer)]
    flatten' v = goOuter [] (Map.toList $ V2.getValue v)
      where
        goOuter acc []             = acc
        goOuter acc ((cs, m) : tl) = goOuter (goInner cs acc (Map.toList m)) tl

        goInner _ acc [] = acc
        goInner cs acc ((tn, a) : tl)
            | a /= 0    = goInner cs ((cs, tn, a) : acc) tl
            | otherwise = goInner cs acc tl

{-# INLINEABLE mustFindScriptDatum #-}
mustFindScriptDatum :: (UnsafeFromData d) => TxOut -> TxInfo -> d
mustFindScriptDatum o info = case V2.txOutDatum o of
  V2.OutputDatum (V2.Datum dat) -> PlutusTx.unsafeFromBuiltinData dat
  V2.OutputDatumHash dh -> case findDatum dh info of
    Just (V2.Datum dat) -> PlutusTx.unsafeFromBuiltinData dat
    _ -> error ()
  _ -> error ()

-- Convert from an integer to its text representation. Example: 123 => "123"
-- (Note: not compatible with bsToInteger as defined below, but that's ok)
{-# INLINEABLE integerToBS #-}
integerToBS :: Integer -> BuiltinByteString
integerToBS x
  | x < 0 = consByteString minusAsciiCode $ integerToBS (negate x)
  -- x is single-digit
  | x `quotient` 10 == 0 = digitToBS x
  | otherwise = integerToBS (x `quotient` 10) <> digitToBS (x `remainder` 10)
  where
    digitToBS :: Integer -> BuiltinByteString
    digitToBS d = consByteString (d + zeroAsciiCode) emptyByteString

-- Convert an ASCII string representing an integer to an integer. Error if invalid.
{-# INLINEABLE bsToInteger #-}
bsToInteger :: BuiltinByteString -> Integer
bsToInteger input = go 0 0
  where
    len = lengthOfByteString input
    go idx acc
      | idx == len = acc
      | otherwise = go (idx + 1) (acc * 256 + indexByteString input idx)

{-# INLINEABLE licenseDeadline #-}
licenseDeadline ::  V2.TxInInfo -> V2.CurrencySymbol -> Integer
licenseDeadline licenseInput licenseSymbol =
  let (V2.Value mp) = V2.txOutValue $ V2.txInInfoResolved licenseInput
      (V2.TokenName license) = case Map.lookup licenseSymbol mp of
        Just i -> case Map.toList i of
          [(tn, _)] -> tn
          _ -> error ()
        _ -> error ()
    in bsToInteger license

{-# INLINEABLE hasOutDatum #-}
hasOutDatum ::  V2.TxOut -> Bool
hasOutDatum o = case V2.txOutDatum o of
  V2.NoOutputDatum -> False
  _ -> True