# MuesliSwapPools

MuesliSwap is originally an order book based DEX. The contracts contained in this repo are meant to enhance the order book functionality by providing additional liquidity to be used for fulfilling orders. As a basis, we build on MinSwaps Liquidity Pool contracts that have been published on March 20th 2022 under GNU GPLv3 license. These contracts will be referred to as the original MinSwap contracts in the following. A number of changes to these contracts have been made -- some to adopt for different requirements while preserving security, some to improve security -- which we describe in the following.

## Changes from MinSwap contracts

### Due to vulnerabilities in MinSwap contract

- **LP token minting issue:** The original MinSwap contract turned out to have a critical vulnerability: Due to a missing check during pool creation, arbitrary amounts of LP tokens could be minted by anyone. The fix, namely checking that the minted value only contains the desired single pool NFT, has subsequently been described by the responsible audit company Tweag in [this blog post](https://www.tweag.io/blog/2022-03-25-minswap-lp-vulnerability/). Our version makes exactly the same changes to the original MinSwap contracts. In addition, and as also done in the revised Minswap version, we further secure pool creation by requiring the creator to present an OWNER license token (which can safely be assumed not to fall into malicious hands) during the factory transaction.

- **Preventing OWNER from arbitrarily collecting user funds locked at batcher contract:** This change concerns the following critical finding we encountered during our inspection of the original Minswap contracts -- an issue that has neither been pointed out in the Tweag audit, nor in any of the aftermath blog posts by Minswap and Tweag. First, it is important to point out how the batching mechanism is implemented in the original MinSwap contract: Deposit, withdraw, and swap orders are locked by users at the batching contract. Batchers can then spend multiple of these order UTXO simultaneously in order to apply them to a pool UTXO. When doing so, the batcher script only checks that a pool UTXO is consumed in the same transaction, outsourcing all additional checks (ensuring that the order is fulfilled and correctly applied) to the pool script. This is a valid design choice as long as the pool script indeed implements all the necessary checks. But for the `UpdateFeeTo` and `WithdrawLiquidityShare` redeemers this is not the case. One may argue that these actions can anyway only be performed by OWNER token holders which can be trusted not to steal user funds. However, in our view, requiring this trust is completely unnecessary. Demanding it turns the product into a centralized solution (if this is accepted one could build a trivial swap by only checking license token ownership) at best, and represents a major vulnerability involving loss of user funds at worst.
[Here](https://testnet.cardanoscan.io/transaction/3353389d4ec34f10b54ebd839ae7e5c70662058283c26dc1cdfd2262d77bff38)'s an `UpdateFeeTo` transaction performed on the testnet version of Minswap's original contract to prove feasibility of the attack.

    Various ways of fixing this are imaginable, we implement the following: A special additional field is included in all datums of order UTXOs locked at the batcher script that identifies our protocol. The condition `hasNoInputFromBatcher` needs to be satisfied for `UpdateFeeTo` and `WithdrawLiquidityShare` transactions. `hasNoInputFromBatcher` is implemented by checking that none of the transactions inputs has the order datum type recognizable in particular by the dedicated extra field. These changes obviously fix the issue by preventing OWNERs from illegally spending batcher UTXOs.

### Due to difference in use case

Due to MuesliSwap's hybrid approach between order book and AMM/LP DEX, the requirements for the pool contracts slightly deviate from what is needed with a purely LP-based DEX. Note that the two models actually work together very naturally: Instead of locking swap orders into an auxiliary batcher contract, MuesliSwap orders are placed in the original order book. They can then be (partially or fully) matched using liquidity from any source such as opposing orders or pools. Therefore, we restrict pool swaps to be carried out by matchmakers and with input value coming from the order book contract. Notice how the order book functions both as a concurrency solution and connection between the two models. For deposit and withdraw orders we preserve the previous way of locking them into a batcher contract and having batchers process batches of such orders. The following changes have been made:

- **Replacing batched user pool swaps with direct matchmaker swaps:** Since only matchmakers perform pool swaps, we can allow direct (i.e. unbatched) interaction with the pool UTXO (the batching implicitly happens by matchmakers fulfilling multiple orders in one transaction). Therefore, a new pool script redeemer `DirectSwap` is introduced. For validating swaps we deviate from the original idea of simulating the application of such orders to the current pool state by simplyfing it to checking wether the constant product formula remains satisfied. The order types `SwapExactIn`, `SwapExactOut` are therefore obsolete and hence removed. Also, to prevent matchmakers from using pool swaps for purposes other than fulfilling order book orders, we check that all value being used for the swap originates from order book script inputs.

- **Parameterized protocol fees:** In the Minswap contracts, the protocol fee is hardcoded to 0.3% (or, with profit sharing, to 0.25% with 0.05% going to the protocol owner) as originally proposed by Uniswap. For our purposes, it is desirable to turn these magic numbers into parameters that can be set during pool creation and that are then stored in the pool UTXO's datum. We also check that these values remain unchanged whenever the pool UTXO is spent. Note that since fee parameters can only be set during creation and are then immutable, the protocol remains fair: Every user can inspect a pool's fees and decide if it's worth using for swaps or providing liquidity to.

- **Additional license types:** As described above, there are now multiple roles in the protocol: matchmaker, batcher, owner, and matchmaker with permission to use pool swaps for satisfying orders (called swapper here). To manage permissions more flexibly we provide separate license tokens for each role. All license types except OWNER tokens will be distributed periodically and therefore have expiration dates encoded in the token name.

### Minor changes

- **Shorter encoding of timestamp in token name:** To encode expiration timestmaps in token names, we need to represent the respective integer as a string. Originally, this has been done by writing out each digit as the corresponding character. Token names become much shorter by instead converting the decimal integer into hexadecimal notation and writing out the chars represented by the hexadecimal digits. This makes both de- and encoding more efficient and else does not change the behaviour.

- **General refactoring** 

## Build instructions

- Making sure `nix` are available on your machine
- Checkout https://github.com/input-output-hk/plutus-apps to commit `c1c65f7873fe184ff54ab25a43aeb8548fe6ff9c` 
- Run `nix-shell --extra-experimental-features flakes` command
- In `nix-shell` go to the `dex` folder
- Run `cabal run muesliswappools` and check your result with script in `plutus` folder
- Run `./build-scripts.sh` and check your result with script address and policyId in `plutus` folder
