# MuesliSwap audit competition

## MuesliSwap’s approach to smart contract security

In order to achieve true decentralization and trustlessness, at MuesliSwap, our primary goal when developing smart contracts is to deliver secure and transparent code. Doing so requires not only our own strong confidence in the correctness of our contracts but also the conviction of the whole community, and especially ways for non-technical users to trade safely. Therefore we distribute trust across a wide range of security measures, namely:

- Relentless testing procedures and internal audits conducted by our team on both the MuesliSwap Orderbook v2 and the MuesliSwap Pools/AMM contracts.
- A rigorous audit by the renowned external Haskell/Plutus experts from Mlabs of our MuesliSwap Orderbook v2 contracts, see [report here](https://github.com/mlabs-haskell/muesliswap-audit-public/blob/master/MuesliSwap-audit-report.pdf).
- [Open-sourced MuesliSwap Orderbook v2 contracts](https://github.com/MuesliSwapTeam/muesliswap-cardano-pool-contracts) that are built on top of the Minswap contracts (with some adjustments regarding functionality and improvements in security) that have been [audited by Tweag](https://www.tweag.io/27d9c5c6f71d11f2f6c6e853a90b42ad/MinSwap-Jan31.pdf) and are successfully operating on mainnet.
- A community audit competition with juicy bug bounties and reward NFTs for every participating community member -- details below.

## About the audit competition

As proud members of the Cardano community, we find ourselves surrounded by a large group of users that thoroughly do their due diligence and critically question the protocols they are interacting with. This is great and necessary for decentralized platforms to be truly trustless. In order to further foster such community effort and distribute trust across as many technically skilled users as possible we came up with the MuesliSwap audit competition: Take at look at the code in this repository and be rewarded with an exclusive MuesliSwap community auditor NFT and juicy bug bounties in the (admittedly unlikely) case of discovering a vulnerability.

## Details and conditions

The security claim we are making about the contracts contained in [this repository](https://github.com/MuesliSwapTeam/muesliswap-cardano-pool-contracts) is the following:

*All funds provided as liquidity to MuesliSwap pools are safe, i.e. when depositing `x` coinA and `y` coinB, there user will receive an amount of LP tokens that will at any later point allow her to withdraw `x’` coinA and `y’` coinB where `x*y = x’*y’`.*

This is the standard behavior of a Uniswap-style AMM-DEX, to learn more have a look at the [Uniswap whitepaper](https://uniswap.org/whitepaper.pdf). Note that in particular, in the context of this competition, the MuesliSwap Orderbook v2 contract should be treated as a black box since its behavior is independent of the above security claim (and has successfully passed the Mlabs audit).

In terms of vulnerabilities and bug bounties we distinguish between:
- **Major vulnerability:** *(bug bounty of 2,000 to 10,000 ADA)* A clear violation of the security claim above, meaning a malicious actor can steal user funds).
- **Minor issue:** *(bug bounty of 100 to 500 ADA)* Issue that violates the security claim above in some special cases. In no circumstance can this issue lead to a significant loss of user funds.

*About the MuesliSwap community audit NFT:* The community audit NFT is a token of appreciation for the effort to increase trust in the MuesliSwap protocol. It is awarded to any user that claims to have looked at the protocol and tried to understand and verify the correctness of its behavior.

## How to participate?

1. Check out the [MuesliSwap AMM code repository](https://github.com/MuesliSwapTeam/muesliswap-cardano-pool-contracts). You will find testnet contract addresses, code, and build instructions there.
2. Try to understand which operations are allowed by understanding the conditions demanded by the contract for spending script UTXOs. Refer to the Haskell Pioneer Program or ask on [Discord](https://discord.gg/vHzSXRWBJR), [Telegram](https://t.me/muesliswapADA) or [Twitter](https://twitter.com/MuesliSwapTeam) for support by us or our community members.
3. You will notice that certain actions (e.g. creating pools, batching deposit/withdrawal orders, swapping with pools) require holding different kinds of license tokens. If you want to do tests involving such actions feel free to reach out to us on [Discord](https://discord.gg/vHzSXRWBJR) and we will be happy to provide the necessary testnet licenses.
4. Feel free to share your findings on our channels or with us directly whether you feel it is important or not.
5. After you have collected your findings and optionally shared them with us, fill out [this form](https://forms.gle/HF2RuSbCrPZmU7fC7) so we can whitelist your wallet address for sending out the community audit NFT.
6. After 12 days (ending on June 20, 23:59 UTC) we will announce the end of the community audit, share our findings, and possibly reward bug bounties. We will also make announcements regarding distribution of the community audit NFT, so stay tuned for our updates!
