#!/usr/bin/env bash
magic="1097911063"

set -euo pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
cd "$here"

cardano-cli address build --payment-script-file plutus/batch_order_script.plutus --mainnet --out-file plutus/batch_order.addr
cardano-cli address build --payment-script-file plutus/pool_script.plutus --mainnet --out-file plutus/pool_without_stake_key.addr
cardano-cli transaction policyid --script-file plutus/factory_minting_policy.plutus >plutus/factory_policyid
cardano-cli transaction policyid --script-file plutus/lp_minting_policy.plutus >plutus/lp_policyid
cardano-cli transaction policyid --script-file plutus/nft_minting_policy.plutus >plutus/nft_policyid

cardano-cli address build --payment-script-file plutus/batch_order_script.plutus --testnet-magic $magic --out-file plutus/testnet/batch_order.addr
cardano-cli address build --payment-script-file plutus/pool_script.plutus --testnet-magic $magic --out-file plutus/testnet/pool.addr
cardano-cli transaction policyid --script-file plutus/factory_minting_policy.plutus >plutus/testnet/factory_policyid
cardano-cli transaction policyid --script-file plutus/lp_minting_policy.plutus >plutus/testnet/lp_policyid
cardano-cli transaction policyid --script-file plutus/nft_minting_policy.plutus >plutus/testnet/nft_policyid