# Credits

Condensed version of Lecture #5 of the Plutus Pioneer Program by Lars Brünjes on [Youtube](https://www.youtube.com/watch?v=6VbhY162GQA)

Cloned from [Reddit (u/RikAlexander)](https://www.reddit.com/r/cardano/comments/ne0nuz/week_05_plutus_pioneer_program/)

## Overview

This lecture is all about Native Tokens!

We'll be looking at how to Mint and Burn tokens, also how to define minting policies; which define under which conditions native tokens can be minted or burned.

## Value

First of all each UTxO has both an `Address` and a `Value`.

In EUTxO, each UTxO also has a `Datum` (for which we have seen multiple use-cases in weeks 1-4).

Up until now `Value` was always Ada (or Lovelaces)

*Note:* one exception being the Auction example from Week01. Although we did create an NFT to be auctioned, we did it by using a Playground feature that created this NFT at Initialization.

We identify tokens by its `CurrencySymbol` and `TokenName`.

For Ada these are both *empty strings*.

If we look at [Plutus/V1/Ledger/Value.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs) of the [Plutus Repository](https://github.com/input-output-hk/plutus)

This combination of `CurrencySymbol` and `TokenName` is also called an `AssetClass` as defined on line 183

```haskell
newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
```

The `Value` type, by setting the Integer, just says: how many units of each `AssetClass` are in this value.

Line 210

```haskell
newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }
```

Although it is important to know what `Value` is, we should never construct a `Value` directly with Maps; there are helper functions for this that should be used.

For Lovelaces, we have the `lovelaceValueOf` function, which takes an `Integer` and returns a `Value`.

As I said, for Ada both the `CurrencySymbol` and `TokenName` are empty strings, for Ada these are defined in [Plutus/V1/Ledger/Ada.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Ada.hs) of the [Plutus Repository](https://github.com/input-output-hk/plutus)

We can use them by calling `adaSymbol` and/or `adaToken`; both of which contain an `emptyByteString`.

Line 47-48

```haskell
adaSymbol :: CurrencySymbol
adaSymbol = TH.currencySymbol emptyByteString
```

Line 52-53

```haskell
adaToken :: TokenName
adaToken = TH.tokenName emptyByteString
```

If we call the `lovelaceValueOf` function with a Integer valued 123, we can see our generated `Value`.

```haskell
lovelaceValueOf 123
Value (Map [(,Map [("",123)])])
```

As you can see, both the `CurrencySymbol` and `TokenName` are empty, by which we know this is a value of 123 lovelaces.

## lovelaceValueOf for other tokens

To create `Value`'s for Tokens other than Ada, we can use `singleton`, it is specified as:

```haskell
singleton :: CurrencySymbol -> TokenName -> Integer -> Value
```

`CurrencySymbol` must be a valid hexadecimal value (for example: `a8ff`) (we'll get to "why" in a second)

`TokenName` can be any arbitrary byte string

For example:

```haskell
singleton "a8ff" "TOKEN" 10
```

would return a `Value` of

```haskell
Value (Map [(a8ff,Map [("TOKEN",10)])])
```

*Note:* to use strings as we do here, we must activate the OverloadedStrings extension, in the ghci we do this by calling: `:set -XOverloadedStrings` in our scripts we would need to add the language pragma `{-# LANGUAGE OverloadedStrings #-}`

## Combining Value's

The `Value` class is an instance of `Monoid` (check out week04 for a more in depth explanation).

Because of this, we can use the `mappend` functionality that Monoids give us, to combine 2 (or more) `Value`'s.

*Note:* the mappend function also has an operator for it: `<>`

Example using `lovelaceValueOf`

```haskell
lovelaceValueOf 15 <> lovelaceValueOf 10
```

Which would give us a `Value` of 25 lovelaces.

When combining multiple `Values` of different Tokens (so not only lovelaces), we'd get a `Value` with multiple Maps. e.g.

```haskell
singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
```

which would return

```haskell
Value (Map [(, Map [("", 42)]), ("a8ff", Map [("ABC", 7), ("XYZ", 100)])])
```

Now if we'd want to retrieve for example our 100 'XYZ', we can do so by using the `valueOf` function

```haskell
valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
```

First for ease of use, we'll assign our two singletons and lovelaces to a variable

```haskell
let v = singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
```

Which we can now use with `valueOf`

```haskell
valueOf v "a8ff" "XYZ"
```

To get our 100 "XYZ".

Last but not least, there is the `flattenValue` function, which.. well.. flattens the `Maps` in our `Value`

```haskell
flattenValue v
```

To get a flattened list of triples

```haskell
[(a8ff,"ABC",7),(a8ff,"XYZ",100),(,"",42)]
```

## Minting Policy

In every transaction, there is a rule.

That rule states: "What comes in, must come out."

*Note:* one exception is the `Fees`

This is important because this would mean we could never mint/create tokens in a transaction, as this would break that rule.

This is where Minting Policies come in.

Our `CurrencySymbol`, in all of our created `Value`'s, was always the hexadecimal string "a8ff", why?

Well the `CurrencySymbol` is the Hash of a Script; our Minting Policy (or `MonetaryPolicy`)

For each transaction that wants to mint or burn native tokens, `CurrencySymbol` is looked up, this script is now executed alongside other validation scripts, that then decides if the transaction is allowed to mint/burn native tokens.

For Ada this applies as well, here our `CurrencySymbol` thus our Minting Policy (or `MonetaryPolicy`), is *empty*.

Because there is no script here, we have no way of minting or burning Ada; which is exactly what we expect. The amount of Ada is fixed, and cannot be changed.

If we look at the definition of ScriptContext in [Plutus/V1/Ledger/Contexts.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs) of the [Plutus Repository](https://github.com/input-output-hk/plutus)

Line 115

```haskell
data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
```

We have a `ScriptPurpose` here (defined on lines 94 - 98)

```haskell
data ScriptPurpose
	= Minting CurrencySymbol
	| Spending TxOutRef
	| Rewarding StakingCredential
	| Certifying DCert
```

Until now we have only been using `ScriptContext`'s with a `ScriptPurpose` of `Spending`; now we'll have the Purpose `Minting`.

`ScriptContext` also has a `scriptContextTxInfo` of type `TxInfo` (lines 101 - 113)

```haskell
data TxInfo = TxInfo
	{ txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
	, txInfoOutputs     :: [TxOut] -- ^ Transaction outputs
	, txInfoFee         :: Value -- ^ The fee paid by this transaction.
	, txInfoForge       :: Value -- ^ The 'Value' forged by this transaction.
	, txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
	, txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
	, txInfoValidRange  :: SlotRange -- ^ The valid range for the transaction.
	, txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
	, txInfoData        :: [(DatumHash, Datum)]
	, txInfoId          :: TxId
	-- ^ Hash of the pending transaction (excluding witnesses)
	} deriving (Generic)
```

For all the examples (weeks) till now, the `txInfoForge` was always 0; though for our Minting Policy, this may contain a `Value` with 1 or more `AssetClass`es (see chapter `Value`).

For each `AssetClass` in `txInfoForge`, the corresponding script is run (`Minting Policy` / `Monetary Policy`).

If we remember from week 3, our Validator script got 3 values to work with: Datum, Redeemer and ScriptContext.

Minting Policies only get the ScriptContext.

Datum and Redeemer would both not make sense; the Datum belongs to a UTxO, and the Redeemer to input, but the Forging (`txInfoForge`) belongs to the transaction, not to a specific Input or Output.

## Free ! (Minting Policy example)

Open the file `Free.hs` in plutus-pioneer-program/code/week05/src/Week05/Free.hs

Lines 34 - 39

```haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: ScriptContext -> Bool
mkPolicy _ = True

policy :: Scripts.MonetaryPolicy
policy = mkMonetaryPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy mkPolicy ||])
```

Here we have defined our most basic `MonetaryPolicy` there could be. (same as we did when we started learning about Validators)

`mkPolicy` only receives the ScriptContext (so no Datum or Redeemer), and we just ignore the ScriptContext input (`_`) and return True! (no validation whatsoever).

Then, same as for our `Validator`'s we'll compile it into Plutus Core by using `mkMonetaryPolicyScript` and inlining our `mkPolicy`.

*Note:* this should all feel quite familiar. If not? Check out [week 02](https://www.reddit.com/r/cardano/comments/mumpdx/week_02_plutus_pioneer_program/) which is all about the Validator\`.

Now we'll create our `CurrencySymbol` (Lines 41 - 42)

```haskell
curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy
```

Great, we're all done with the on-chain part.

For the off-chain part, we want to be able to Mint and/or Burn tokens of this `CurrencySymbol`.

For doing so, we still need our `TokenName`, and a amount (Integer) to either mint or burn. (`Value` consists of those three values)

Lines 44 - 51

```haskell
data MintParams = MintParams
	{ mpTokenName :: !TokenName
	, mpAmount    :: !Integer
	} deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema =
	BlockchainActions
		.\/ Endpoint "mint" MintParams
```

We'll create a new Endpoint with both our missing arguments (TokenName / Amount)

If the Amount is a positive Integer, we would be Minting Tokens, if it's negative we would be Burning them.

Time to write the contract itself.

Lines 53 - 60

```haskell
mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
	let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
		lookups = Constraints.monetaryPolicy policy
		tx      = Constraints.mustForgeValue val
	ledgerTx <- submitTxConstraintsWith @Void lookups tx
	void $ awaitTxConfirmed $ txId ledgerTx
	Contract.logInfo @String $ printf "forged %s" (show val)
```

Lets go through it.

First our Contract return type arguments:

* `w` \-> Tell/Writer, is not used here, so not specified (hence it being written in lowercase to indicate exactly that)
* `FreeSchema` \-> `BlockchainAction`'s and access to our endpoint(s) (in this case the "mint" endpoint we've previously created)
* `Text` \-> Type of error messages
* () -> Void / Unit, returns nothing.

Secondly, our `val` which is our `Value` (created with `Value.singleton`) with our `CurrencySymbol` (curSymbol) and both the TokenName and the Amount specified through the endpoint "mint".

Now in order to create a Transaction (`tx` in our script), instead of creating this transaction ourselves, which would be very tedious, as it would require us to compute the fees, and a whole lot of other things.

Another (better/easier) approach was taken.

We define constraints. In other words, we define conditions which this transaction must fulfill, and let the library do all the tedious work for us.

All of these constraints-creation functions, start with "must" e.g. `mustForgeValue`, `mustPayToPublicKey` etc.

`tx` is `Constraints.mustForgeValue` (our `val`), which will give us a transaction that we can now send to the chain via `submitTxConstraintsWith`.

Important to note: `submitTxConstraintsWith` may fail. For example if we don't have enough funds.

`submitTxConstraintsWith` also uses the lookups variable, it is there to include additional information to (hopefully) validate our `Constraints`.

In this case, lookups contains our MonetaryPolicy (the one that always returns True for now).

We need to do this, because our create `Value` (`val`), only contains the Hash of our Policy script, but in order to validate this transaction, we need the Script itself (not just the hash), so that we can execute it.

Other possible lookups: UTxOs, validation scripts

Last but not least the `awaitTxConfirmed` function, waits for our transaction (`LedgerTx`) to confirm.

*Note:* if this transaction fails to validate, for now this line blocks forever. This will change in the near-future though, as the Plutus team will provide more handling here (e.g. detect if this has failed or not)

At the end of this script we'll just log how many of the Token we have forged.

Lines 62 - 69 contains some more boilerplate code mostly for the playground (which you, if you followed along with weeks 1-4, should be familiar with)

```haskell
endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
	where
	mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []
```

## Testing

For testing our simple contract, we could do two things, either run it in the playground, or write custom tests which we can run in our terminal.

## Playground

Copy the lines 1 - 69 of `Free.hs` into the Playground and compile them.

Now setup 2 wallet with both 1000 lovelaces.

![Wallets (Free.hs)](https://preview.redd.it/5td4y4tmekz61.png?width=641&format=png&auto=webp&s=34cd3928f48cbe35df3bed0315cf3e35c533ea88)

And a few actions to test:

![Actions (Free.hs)](https://preview.redd.it/9ibu4k2pekz61.png?width=639&format=png&auto=webp&s=235770178b73c456de195a0c9e20ae34f6ea471a)

Now if we evaluate this.

* Wallet 1 will mint 555 of the ABC token.
* Wallet 2 will mint 444 of the ABC token.
* We'll wait 1 slot.
* Wallet 1 will burn 222 of the ABC token.
* Wait another 1 slot.

Everyone of these transactions will cost a fixed total of 10 lovelaces, although this will change in the near-future to a calculated amount of lovelaces.

![Final Balances Playground](https://preview.redd.it/2da49atqekz61.png?width=679&format=png&auto=webp&s=565c14595064d639a12bfbdb8afa3d52d9240e5c)

## EmulatorTrace

Lines 71 - 89 of Free.hs

```haskell
test :: IO ()
test = runEmulatorTraceIO $ do
	let tn = "ABC"
	h1 <- activateContractWallet (Wallet 1) endpoints
	h2 <- activateContractWallet (Wallet 2) endpoints
	callEndpoint @"mint" h1 $ MintParams
		{ mpTokenName = tn
		, mpAmount    = 555
		}
	callEndpoint @"mint" h2 $ MintParams
		{ mpTokenName = tn
		, mpAmount    = 444
		}
	void $ Emulator.waitNSlots 1
	callEndpoint @"mint" h1 $ MintParams
		{ mpTokenName = tn
		, mpAmount    = -222
		}
	void $ Emulator.waitNSlots 1
```

Of course here we do exactly the same, only without the use of the playground.

For more information about the `EmulatorTrace` and testing, check out [week04](https://www.reddit.com/r/cardano/comments/n9c9wz/week_04_plutus_pioneer_program/).

## Realistic example (Signed)

For a more realistic example, we will use the `Free.hs` script as a starting point, only this time we will restrict minting and burning by transactions signed by a specified public key hash. (kind of like a central bank)

Open the file `Signed.hs` in plutus-pioneer-program/code/week05/src/Week05/Signed.hs

Lines 35 - 37

```haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> ScriptContext -> Bool
mkPolicy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh
```

The policy now has the extra parameter `PubKeyHash`, which we need to check if this transaction was signed by.

`ScriptContextTxInfo` in our `ScriptContext` (ctx) contains all the signatories of the transaction; by using `txSignedBy` we can check if our `PubKeyHash` is in this list.

`txSignedBy` takes the `TxInfo` and a `PubKeyHash` and returns a boolean.

```haskell
txSignedBy :: TxInfo -> PubKeyHash -> Bool
```

Lines 39 - 46

```haskell
policy :: PubKeyHash -> Scripts.MonetaryPolicy
policy pkh = mkMonetaryPolicyScript $
	$$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . mkPolicy ||])
	`PlutusTx.applyCode`
	PlutusTx.liftCode pkh

curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy
```

Here we are just parameterizing our `PubKeyHash` (for more information check out [week 3](https://www.reddit.com/r/cardano/comments/n3evg4/week_03_plutus_pioneer_program/) chapter Parameterized)

Lines 57 - 65

```haskell
mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
	pkh <- pubKeyHash <$> Contract.ownPubKey
	let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
		lookups = Constraints.monetaryPolicy $ policy pkh
		tx      = Constraints.mustForgeValue val
	ledgerTx <- submitTxConstraintsWith @Void lookups tx
	void $ awaitTxConfirmed $ txId ledgerTx
	Contract.logInfo @String $ printf "forged %s" (show val)
```

to get the `pubKeyHash` of our contract, we'll use the `pubKeyHash` function together with our `Contract.ownPubKey`.

Using the `<$>` operator, which is just a synonym for `fmap`. [Source](https://stackoverflow.com/questions/37286376/what-does-mean-in-haskell/37286470)

If we now run this script (Signed.hs) in either the Playground or by using our EmulatorTrace tests, we can see that for wallet 1 and wallet 2, there are 2 different tokens.

```bash
Final balances
Wallet 1:
	{, ""}: 99999980
	{7183b1cf81e44b26c558ddf442c4a7161a1b504b61136a8773dc2e4960323521, "ABC"}: 333
Wallet 2:
	{2a964fa6314803cf1b61165aeb1d758e355aae9480a29e282b58e76983f101ba, "ABC"}: 444
	{, ""}: 99999990
```

The lovelaces went down by 10 because of the fees.

Wallet 1 has 333 of the ABC Token, but even though it has the same TokenName, the CurrencySymbol is totally different. (because now it is based on our PublicKeyHash).

Same for wallet 2 only with an amount of 444.

Simply put.

Only wallet 1 can ever Mint or Burn the ABC Token with CurrencySymbol "7183b1cf81e44b26c558ddf442c4a7161a1b504b61136a8773dc2e4960323521".

And only wallet 2 can ever Mint or Burn them with CurrencySymbol "2a964fa6314803cf1b61165aeb1d758e355aae9480a29e282b58e76983f101ba".

## NFT (Non-fungible Token)

The infamous NFT: Tokens that can exist only ONCE. There is only one of each in existence.

## Option 1

With our current knowledge we could, in the Policy look at the Forge Field (`txInfoForge`), and add a condition that only one token is forged.

But that won't do us any good, because that would mean during one transaction we could only mint one token. It wouldn't stop us from doing multiple transactions though.

## Option 2

Another option, one that is already used right now on the Cardano Blockchain (since Mary) to create NFT's, is by setting a deadline; after which nobody is allowed to mint or burn any more tokens.

We could then mint 1, wait until the deadline has passed, and there you go. An NFT.

The problem with this is, to check if an NFT is really an NFT, we'd have to check the blockchain (with a blockchain explorer), which is not that easy...

## Plutus power (Is it a bird... Is it a plane?.. NO. It's Plutus.) 😂

Using Plutus though we can create REAL NFTs.

If you know the policy script that corresponds to the currency symbol; you can be sure, that only one such token can be in existence. (without having to use something like a blockchain explorer)

To not have the same problem that we had in option 1 (doing multiple transactions for minting that would all validate), we have to have some unique value, that can never be used again, thus never allowing minting of this token again.

For this we can use the UTxO. Which is ALWAYS unique.

By only allowing one UTxO to mint, we will have a true NFT.

UTxO -> Output of a transaction; Transaction ID + Index of the output of the transaction.

Because transactions are unique as well, so is our UTxO.

Now why are transactions unique?

Interestingly they wouldn't be, if it wasn't for fees.

Without them, we could have a transaction with 0 inputs, and only outputs without value.

Such a transaction could exist several times; but because of the fees, we always need an input to provide the funds to pay the fees for the transaction.

And as soon as we have an input, that input must come from somewhere, it must come from a previous UTxO; thus, as soon as we have fees, we can never have the same UTxO more than once.

## Plutus NFT Script

Open the file `NFT.hs` in plutus-pioneer-program/code/week05/src/Week05/NFT.hs

Lines 34 - 48

```haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> ScriptContext -> Bool
mkPolicy oref tn ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
						traceIfFalse "wrong amount minted" checkMintedAmount
	where
	info :: TxInfo
	info = scriptContextTxInfo ctx

	hasUTxO :: Bool
	hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

	checkMintedAmount :: Bool
	checkMintedAmount = case flattenValue (txInfoForge info) of
		[(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
		_                -> False
```

We will be parameterizing both `TxOutRef` and `TokenName` to be used in our policy.

`TxOutRef` is a value in `TxInfo` from our `ScriptContext`:

```haskell
type TxInfo :: *
data TxInfo
	= TxInfo {txInfoInputs :: [TxInInfo],
		...
```

`TxInInfo` in `txInfoInputs` is defined as:

```haskell
type TxInInfo :: *
data TxInInfo
	= TxInInfo {txInInfoOutRef :: TxOutRef, txInInfoResolved :: TxOut}
```

There is our TxOutRef, which in other words is an identifier for our UTxO, which is exactly what we need.

Our defined `hasUTxO` function, checks if the TxOutRef provided as parameter to the policy, is in our `txInfoInputs`, by using the `any` function. If not, throw an error with `traceIfFalse`.

```haskell
any :: Foldable t => (a -> Bool) -> t a -> Bool
```

Lars gives us an example how `any` could be used (which is pretty self explanatory):

```haskell
any even [2 :: Int, 3, 5, 7]
True

any even [1 :: Int, 3, 5, 7]
False
```

Now with the `checkMintedAmount` function, we will limit the amount of possible minted tokens to 1. Guaranteeing an NFT. (also with `traceIfFalse` for error handling)

We'll use `flattenValue` to receive a flattened list of triples for all values in the `txInfoForge` variable, (Chapter Combining `Value`'s), which we'll compare to our custom list containing (CurrencySymbol, TokenName, Amount), in this case (cs, tn, 1) (the amount 1 is the important thing here, which is going to prevent anyone of minting more than one of this token).

We do have one Chicken-egg-problem here: our `CurrencySymbol`.

This hash is computed from the policy script, but in order to run this script, we first need the `CurrencySymbol`...

For this we have the function `ownCurrencySymbol`, with this we get access to the `CurrencySymbol` during execution of our Policy Script.

We just need to feed it our ScriptContext, and it'll give us the correct `CurrencySymbol`.

Lines 50 - 59

```haskell
policy :: TxOutRef -> TokenName -> Scripts.MonetaryPolicy
policy oref tn = mkMonetaryPolicyScript $
	$$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMonetaryPolicy $ mkPolicy oref' tn' ||])
	`PlutusTx.applyCode`
	PlutusTx.liftCode oref
	`PlutusTx.applyCode`
	PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn
```

Here we just parameterize both our oref and tn (TxOutRef / TokenName).

We're done with the on-chain part.

For the off-chain part we need the Contract / Endpoints:

Lines 61 - 63

```haskell
type NFTSchema =
	BlockchainActions
		.\/ Endpoint "mint" TokenName
```

For the endpoints we now only need the TokenName.

Lines 65 - 77

```haskell
mint :: TokenName -> Contract w NFTSchema Text ()
mint tn = do
	pk    <- Contract.ownPubKey
	utxos <- utxoAt (pubKeyAddress pk)
	case Map.keys utxos of
		[]       -> Contract.logError @String "no utxo found"
		oref : _ -> do
			let val     = Value.singleton (curSymbol oref tn) tn 1
				lookups = Constraints.monetaryPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
				tx      = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput oref
			ledgerTx <- submitTxConstraintsWith @Void lookups tx
			void $ awaitTxConfirmed $ txId ledgerTx
			Contract.logInfo @String $ printf "forged %s" (show val)
```

Because a wallet can only spent it's own UTxOs, the UTxO used for our NFT restrictions, must be done via a UTxO at our own address.

This can be done with `utxoAt` defined as (in Plutus.Contract):

```haskell
Address -> Contract w s e Ledger.AddressMap.UtxoMap
```

Basically when calling this with an address, we'd get a UtxoMap back, which contains all the UTxOs at this address.

For our purpose (since all UTxOs are unique), it doesn't matter which one we pick; anyone is fine.

Now whats an 'address', we don't really need that now. All we need to now is we can call the function `pubKeyAddress` which will turn a Public Key into an Address; which is exactly what we need.

`pk <- Contract.ownPubKey` will contain our public key, which we will then turn into an address using the `pubKeyAddress`, running it through the `utxoAt` function, to get a UtxoMap with all our UTxOs; since we don't need all the values it provides, but only the UTxO keys, we'll use the Maps.keys function to only get the UTxO Keys.

We'll start a `case`, where if the UtxoMap is empty, just return an error

```haskell
[] -> Contract.logError @String "no utxo found"
```

Now if we look back at our constraints-creation functions; they all start with "must" e.g. `mustForgeValue`, `mustPayToPublicKey` etc.

We already used `mustForgeValue`, now we need the `mustSpendPubKeyOutput` constraint, defined as:

```haskell
mustSpendPubKeyOutput :: TxOutRef -> TxConstraints i o
```

It takes an TxOutRef and returns a constraint. Awesome. Exactly what we need.

Since we already have the `mustForgeValue` constraint, the question is: "how do we combine constraints?".

Constraints are of type `Semigroup`, which lets us combine multiple `Semigroup`s by using the `<>` operator.

```haskell
tx = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput
```

## Testing

Lines 88 - 95

```haskell
test :: IO ()
test = runEmulatorTraceIO $ do
	let tn = "ABC"
	h1 <- activateContractWallet (Wallet 1) endpoints
	h2 <- activateContractWallet (Wallet 2) endpoints
	callEndpoint @"mint" h1 tn
	callEndpoint @"mint" h2 tn
	void $ Emulator.waitNSlots 1
```

These are just simplified, we just removed.. well.. almost everything. Fixed value for our TokenName and no way to specify the amount.

If we now run our test (which you of course could also run in the Playground), it would return:

```bash
Final balances
Wallet 1:
	{9d969e597d45fcd1732ce255e12a97599e883f924b4565fc3a2407bc08d34524, "ABC"}: 1
	{, ""}: 99999990
Wallet 2:
	{913f220c3b1ba49531bae2fedd9edb138a8b360e7e605bfcf4ff3f2045433069, "ABC"}: 1
	{, ""}: 99999990
```

Real NFTs! Congratulations!

## Footnote

Week05 Done!

Happy Coding! 😊

# Credits

Condensed version of Lecture #5 of the Plutus Pioneer Program by Lars Brünjes on [Youtube](https://www.youtube.com/watch?v=6VbhY162GQA)

Cloned from [Reddit (u/RikAlexander)](https://www.reddit.com/r/cardano/comments/ne0nuz/week_05_plutus_pioneer_program/)
