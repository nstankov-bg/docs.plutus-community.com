# Lecture #3 - Week03 Plutus Pioneer Program

## Credits

Condensed version of Lecture #3 of the Plutus Pioneer Program by Lars Brünjes on [Youtube](https://www.youtube.com/watch?v=Lk1eIVm_ZTQ)
Cloned from [Reddit (u/RikAlexander)](https://www.reddit.com/r/cardano/comments/n3evg4/week_03_plutus_pioneer_program/)


## Changes

To start this lecture, we'll discuss some changes to Plutus.

If we look at the week02 example:

    plutus-pioneer-program/code/week01/src/Week02/IsData.hs


Specifically the Validator:

Lines 37 - 39 of IsData.hs

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: () -> MySillyRedeemer -> ValidatorCtx -> Bool
mkValidator () (MySillyRedeemer r) _ = traceIfFalse "wrong redeemer" $ r == 42
```

What changed?
`ValidatorCtx` was changed to: `ScriptContext`

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: () -> MySillyRedeemer -> ScriptContext -> Bool
mkValidator () (MySillyRedeemer r) _ = traceIfFalse "wrong redeemer" $ r == 42
```

We'll discuss the `ScriptContext` in a second.

Another change:
Lines 53 - 60 of IsData.hs

```haskell
validator :: Validator
validator = Scripts.validatorScript inst

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash
```

Instead of having to create the validator hash ourselves

```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator
```

to then generate our script address

```haskell
scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash
```

We can now use the function `scriptAddress` (notice the lowercase `s`) with our validator as argument

```haskell
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

Which will save us a few lines of code

```haskell
validator :: Validator
validator = Scripts.validatorScript inst

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

## ScriptContext

ScriptContext is defined in [Contexts.hs of the Plutus Repo](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs)

We'll start with it's definition.

Line 116

```haskell
data ScriptContext = ScriptContext{
    scriptContextTxInfo :: TxInfo,
    scriptContextTxInfo :: ScriptPurpose
}
```

The ScriptContext constructor takes a `scriptContextTxInfo` of type `TxInfo`, and a `scriptContextTxInfo` of type `ScriptPurpose`

The `ScriptPurpose` lines 94 - 98

```haskell
data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert
```

The `ScriptContext` can have a Minting, Spending, Rewarding or Certifying purpose.
For this lecture though we'll be using the Spending purpose.

Our `scriptContextTxInfo`, TxInfo is quite extensive, and holds multiple values; which in turn can be used by our validator for use in the validation logic.

Lines 101 - 114

```haskell
data TxInfo = TxInfo
    { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
    , txInfoInputsFees  :: [TxInInfo]     -- ^ Transaction inputs designated to pay fees
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

Most of which are clearly explained in the respective comment next to it.

Although we'll be having a closer look at the `txInfoValidRange` and `txInfoSignatories`.

`txInfoSignatories` holds multiple PubKeyHashes of all wallets that are part of this contract transaction; this is important to check if everybody is who they say they are.

`txInfoValidRange` holds a `SlotRange` which defines at which time the Transaction is valid e.g. you might want to hold ADA until a specific time at which one might redeem it, or maybe you'd want to expire the option of redeeming, etc.

## SlotRange

In our `txInfoValidRange` we have a `SlotRange`, this is defined in [Slot.hs of the Plutus Repo](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Slot.hs)


Defined on line 56

```haskell
type SlotRange = Interval Slot
```

`Slot` is nothing but a wrapper around Integer (in essence Slot is just a number)
(defined on line 42)

`SlotRange` holds and `Interval` (defined in [Interval.hs of the Plutus Repo](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Interval.hs))

Line 58

```haskell
data Interval a = Interval { ivFrom :: LowerBound a, ivTo :: UpperBound a }
```

It is basically nothing else but a range of slots. From -> To.

_Note:_ The interval can be unbounded on either side, `From` doesn't have to be a specific slot, it may also be the beginning of time (Line 63, NegInf) although in reality this wouldn't surpass our Genesis slot, as this is our "beginning" slot; same goes for `To` which may also be defined as the End of time respectively. (Line 63, PosInf)

## Interval

To construct an Interval there are some functions defined in Interval.hs, including:

Excerpts from Lines 186 - 217

```haskell
interval :: a -> a -> Interval a
```

@interval a b@ includes all values that are greater than or equal to @a@ and smaller than @b@. Therefore it includes @a@ but it does not include @b@.

```haskell
singleton :: a -> Interval a
```

As the name suggests, this is just the one slot (from @a@ to @a@)

```haskell
from :: a -> Interval a
```

@from a@ is an 'Interval' that includes all values that are greater than or equal to @a@.
So from @a@ to infinity

```haskell
to :: a -> Interval a
```

@to a@ is an 'Interval' that includes all values that are smaller than @a@.
(Beginning of time -> @to)

```haskell
always :: Interval a
```

An 'Interval' that covers every slot.

```haskell
never :: Interval a
```

An 'Interval' that is empty; hence the name `never`.

-------------------------------------------------------

All of the previous functions are there to assist with easy `Interval` creation.

Now that we know how to create our Intervals, we want to be able to do some checks on it.

Excerpts from lines 219 - 264 of Interval.hs

```haskell
member :: Ord a => a -> Interval a -> Bool
```

Check whether a value is in an interval.

```haskell
overlaps :: Ord a => Interval a -> Interval a -> Bool
```

Check whether two intervals overlap, that is, whether there is a value that is a member of both intervals.

```haskell
intersection :: Ord a => Interval a -> Interval a -> Interval a
```

'intersection a b' is the largest interval that is contained in 'a' and in 'b', if it exists.

```haskell
hull :: Ord a => Interval a -> Interval a -> Interval a
```

'hull a b' is the smallest interval containing 'a' and 'b'.

```haskell
contains :: Ord a => Interval a -> Interval a -> Bool
```

@a `contains` b@ is true if the 'Interval' @b@ is entirely contained in @a@. That is, @a `contains` b@ if for every entry @s@, if @member s b@ then @member s a@.

```haskell
isEmpty :: Ord a => Interval a -> Bool
```

Check if an 'Interval' is empty.

```haskell
before :: Ord a => a -> Interval a -> Bool
```

Check if a value is earlier than the beginning of an 'Interval'.

```haskell
after :: Ord a => a -> Interval a -> Bool
```

Check if a value is later than the end of a 'Interval'.

_Note:_ all of this can of course be found in Interval.hs, but for completeness i've written it all down in this article.

## Interval usage / testing

For testing the Interval creation and the helper functions, go into your Terminal and cd into the week03 directory of the Plutus Pioneer Program repo.

```bash
cd /plutus-pioneer-program/code/week03
```

and run

```bash
cabal repl
```

Which will then give us a interactive environment CLI where we can test `Interval`.

First we'll need to import the `Interval` module by running

```haskell
import Plutus.V1.Ledger.Interval
```

Now we can test our Interval creation by for example running

```haskell
a = interval 10 20
b = from 10
```

which if displayed (just entering the variable name with \<enter\>)

a -> an Interval from slot 10 to 20

b -> an Interval from slot 10 to infinity

Play around with all the possible constructor/helper functions! :)

## Vesting

Open the file

    /plutus-pioneer-program/code/week03/Vesting.hs

Copy all its contents into the Plutus Playground

Either use your own local running playground.
Instructions located: [Community Docs](http://docs.plutus-community.com) or [my Reddit article for MacOS](https://www.reddit.com/r/cardano/comments/mmzut6/macos_plutus_playground_build_instructions/)

You could use the Community Run Playground for week03.

https://playground-week3.plutus-community.com

In short what this contract does is it allows a wallet to send ada, which will be stored, and may only be grabbed after a specific amount of time (slots) and only by a specific wallet.

As of week03, the playground will now also add Fees for transactions.
These are set to 10 Lovelaces (in the future they will probably be more accurately calculated).

So to be sure of having enough Lovelaces to cover the transaction fees, I'd always up my opening balances to something more than 10.

![Wallets](https://preview.redd.it/dmga5nidlrw61.png?width=640&format=png&auto=webp&s=a94131efe2984eacb7e3d87cc780b1a086924e79)

Next we'll set up an example like this:

![Wallets](https://preview.redd.it/opr1f1celrw61.png?width=644&format=png&auto=webp&s=e9b2e189621be563ad6a7d3bcf38c8c8677036dc)

As you can see the `getPubKeyHash` is empty here.

This is where we'll need to input the Public key hash of Wallet 2; by which we will identify if its really wallet2 that tries to grab the funds after the deadline of 10 slots.

How do we get the public key hash for wallet 2? There are 2 ways.
The easiest one would be to just Evaluate the script like this (leaving the pubkeyhash empty)

In the simulated transactions overview, in the genesis slot (0), on the right side where the outputs are located, we can see the pubkeyhash for both wallets.

![Wallets](https://preview.redd.it/9c57qygglrw61.png?width=287&format=png&auto=webp&s=1bdaca5f68f9949f47c17aed07c6506f1b36d212)

The other way, uses the CLI we opened for Interval / testing.

Open it, and first import 2 dependencies:

```haskell
import Wallet.Emulator
```

Which will provide us with the `walletPubKey` function, that takes a wallet and returns a public key for it.

```haskell
:t walletPubKey
walletPubKey :: Wallet -> PubKey
```

Second import:

```haskell
import Ledger
```

This gives us access to the `pubKeyHash` function, that generates a hash based on a wallets public key

```haskell
:t pubKeyHash
pubKeyHash :: PubKey -> PubKeyHash
```

By chaining these two functions, we can easily retrieve the public key hash for wallet 2

```haskell
pubKeyHash $ walletPubKey $ Wallet 2
```

*Note*: we use a dollar sign here, but we could of course also write this as:

```haskell
pubKeyHash (walletPubKey (Wallet 2))
```

Now that we have the public key hash for wallet 2, just copy it, paste it in the getPubKeyHash give action, and reevaluate the script.

The genesis slot (slot 0) just gives both wallets its initial 1000 Lovelaces

![Outputs Genesis](https://preview.redd.it/7nd2j8vilrw61.png?width=284&format=png&auto=webp&s=9b2526b215749b269870befcdc2f85cd5632e971)

Slot 1 will put 100 Lovelaces up for grabs (but only for wallet 2, and only after the specified 10 slots have passed)

Also notice the 10 Lovelaces fee. Now wallet 1 has a balance of 1000 - fee 10 - vesting 100 = 890 Lovelaces.

And the Script has the 100 Lovelaces.

![Outputs Slot 1](https://preview.redd.it/1ju2kdaklrw61.png?width=287&format=png&auto=webp&s=82270151401ba2ce1af4af2053396648d6f2a02a)

Now we've waited 10 Slots, so wallet 2 should be able to retrieve its 100 Lovelaces from the script.

![Outputs Slot 11](https://preview.redd.it/pn58g5hllrw61.png?width=290&format=png&auto=webp&s=9ea654610efc57f5bd438269d2d947123158bd0d)

The Grab action itself also has a Fee of 10 Lovelaces which will be subtracted from the final output. (We're left with 90 Lovelaces)

## Vesting - The code

Great it works. But how does it work?
Well, take a look at Vesting.hs (/plutus-pioneer-program/code/week03/Vesting.hs)

Lines 42 - 54

```haskell
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx =
    traceIfFalse "beneficiary's signature missing" checkSig      &&
    traceIfFalse "deadline not reached"            checkDeadline
where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkSig :: Bool
    checkSig = beneficiary dat `elem` txInfoSignatories info

    checkDeadline :: Bool
    checkDeadline = from (deadline dat) `contains` txInfoValidRange info
```

Our new Validator.

As always the validator receives 3 parameters, first the datum, the redeemer, and the script context.

The datum, of type VestingDatum is defined on lines 34 - 38

```haskell
data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: Slot
    } deriving Show
```

And holds the beneficiary (public key hash of wallet 2) and the deadline (in our case this would be Slot 10).

These must be compared against the data in ScriptContext, which as previously discussed holds quite a lot of data, including txInfoSignatories and txInfoValidRange.

Our validator does 2 checks

```haskell
mkValidator dat () ctx =
    traceIfFalse "beneficiary's signature missing" checkSig      &&
    traceIfFalse "deadline not reached"            checkDeadline
```

By combining them with `&&`, if either one fails, the validator will return False (and thus the grab will fail).

The 2 functions `checkSig` and `checkDeadline` are implemented in the lines after the `where` statement; both of them just return a Bool (True/False)

*Note:* If you're used to SQL, I like to look at the `where` statement as prepared statement bindings. Where we can define the functions more or less after we've said what to do with it's results.

Lines 47 - 48

```haskell
info :: TxInfo
info = scriptContextTxInfo ctx
```

Sets `info` to the `TxInfo` values of our ScriptContext (ctx)

*Note:* for information about the `ScriptContext` and `TxInfo`check the ScriptContext chapter above.

Lines 50 - 51

```haskell
checkSig :: Bool
checkSig = beneficiary dat `elem` txInfoSignatories info
```

Checks if the beneficiary is in our `txInfoSignatories` list (`elem` is an infix function for checking if an item exists in a list).

*Note:* we use an infix function here more for readability than anything else.

We could also write this as `elem $ beneficiary dat $ txInfoSignatories info` or `elem (beneficiary dat) (txInfoSignatories info)`

Lines 53 - 54

```haskell
checkDeadline :: Bool
checkDeadline = from (deadline dat) `contains` txInfoValidRange info
```

Checks if the from (see chapter Interval) slot of the deadline Interval, is contained (see also chapter Interval) in our `txInfoValidRange`.


## Parameterized

Open the file

    /plutus-pioneer-program/code/week03/Parameterized.hs

This script really does exactly the same as our previous script (Vesting).

Although its key difference is that instead of our `VestingDatum` which hold our beneficiary and deadline.

We now call our validator with an extra parameter, in this case `VestingParam`.

It contains the same data as previously in `VestingDatum`, only now by parameterizing this, we get one key difference.

Without the parameterized functionality, there would only be one instance of this smart contract; now, we could easily have multiple instances of the same contract only with different parameters (our `VestingParam`)

Line 44

```haskell
mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
```

What once was our `VestingDatum`, is now just Unit (void), and another parameter is added `VestingParam`.

Lines 58 - 61

```haskell
data Vesting
instance Scripts.ScriptType Vesting where
    type instance DatumType Vesting = ()
    type instance RedeemerType Vesting = ()
```

Here our Datum is also just Unit (we no longer need it)

Lines 63 - 68

```haskell
inst :: VestingParam -> Scripts.ScriptInstance Vesting
inst p = Scripts.validator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
where
    wrap = Scripts.wrapValidator @() @()
```

The important change here plutus compile of our validator

```haskell
($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
```

PlutusTx.compile compiles our validator into Plutus code; the problem solved here, is that if we'd just added `p` (our VestingParam) to it, it would try to compile it into plutus code.
Which wouldn't do us any good, since the value of `p` should be specified when running the script, not when compiling it.

This is solved with `PlutusTx.applyCode` and `PlutusTx.liftCode`.

Lars goes into this with a very good and more in depth explanation, which I would definitely recommend watching ( in the lecture at minute 53:00 )

Mostly though, this would probably be more or less boilerplate code.
So for now I'll just leave it at that.

Lines 70 - 71

```haskell
validator :: VestingParam -> Validator
validator = Scripts.validatorScript . inst
```

Here we receive our VestingParam, and use the `.` to compose the two functions.

Same goes for generating our address

Lines 73 - 74

```haskell
scrAddress :: VestingParam -> Ledger.Address
scrAddress = scriptAddress . validator
```

Difference between `$` and `.`?

The `$` operator is for avoiding parentheses. Anything appearing after it will take precedence over anything that comes before.

The primary purpose of the `.` operator is not to avoid parentheses, but to chain functions. It lets you tie the output of whatever appears on the right to the input of whatever appears on the left. This usually also results in fewer parentheses, but works differently.

(Source: [StackOverflow Question](https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign))



## Homework 1

Open the file

    /plutus-pioneer-program/code/week03/Homework1.hs

Lines 45 - 46

```haskell
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator _ _ _ = False -- FIX ME!
```

Our assignment reads:

    This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
    or if beneficiary2 has signed the transaction and the deadline has passed.

First have a look at our VestingDatum

```haskell
data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: Slot
    } deriving Show
```

We now have 2 beneficiaries

`beneficiary1` -> wallet 2 in this case, the wallet we are sending the ada to redeem it before the deadline has passed.

`beneficiary2`-> wallet 1 in this case, the wallet that send the ada, if its not retrieved before the deadline has passed, we would like to get our ada back! (otherwise it would be lost forever in this contract)

The solution provided by Lars

    /plutus-pioneer-program/code/week03/Solution1.hs

Implements new validation logic

```haskell
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx
    | (beneficiary1 dat `elem` sigs) && (to       (deadline dat) `contains` range) = True
    | (beneficiary2 dat `elem` sigs) && (from (1 + deadline dat) `contains` range) = True
    | otherwise                                                                    = False
where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sigs :: [PubKeyHash]
    sigs = txInfoSignatories info

    range :: SlotRange
    range = txInfoValidRange info
```

It basically checks if the trying to grab the funds is beneficiary 1 or 2 first

    (beneficiary1 dat `elem` sigs)

If this evaluates to True, for beneficiary1 (the one we hoped would grab the funds before the deadline), we check with `to` (see chapter Interval) if we are still in time. (same as we did in the Vesting script with our `checkDeadline` function).

For beneficiary2 we check if the deadline has already passed.

## Homework 2

Homework 2 is really quite easy.

First I would always recommend trying to create your own solutions for the Homework.

But for the sake of this article, I'm just going to provide you with the Solution, and a small explanation.

Open the file

    /plutus-pioneer-program/code/week03/Homework2.hs

Lines 36 - 51

```haskell
mkValidator :: PubKeyHash -> Slot -> () -> ScriptContext -> Bool
mkValidator _ _ _ _ = False -- FIX ME!

data Vesting
instance Scripts.ScriptType Vesting where
    type instance DatumType Vesting = Slot
    type instance RedeemerType Vesting = ()

inst :: PubKeyHash -> Scripts.ScriptInstance Vesting
inst = undefined -- IMPLEMENT ME!

validator :: PubKeyHash -> Validator
validator = undefined -- IMPLEMENT ME!

scrAddress :: PubKeyHash -> Ledger.Address
scrAddress = undefined -- IMPLEMENT ME!
```

In essence we have to implement a validator with a parameter (PubKeyHash) and a Datum which contains the Slot.

In parameterized.hs both these values were located in VestingParam, the real difference is that now they are both individual values, one as parameter, and one as the datum.

So for the solution, we can use the Validator from parameterized.hs only with the PubKeyHash parameter and Slot datum instead of the VestingParam data type that contained both these values.

```haskell
mkValidator :: PubKeyHash -> Slot -> () -> ScriptContext -> Bool
mkValidator pkh s () ctx =
    traceIfFalse "beneficiary's signature missing" checkSig      &&
    traceIfFalse "deadline not reached"            checkDeadline
where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkSig :: Bool
    checkSig = pkh `elem` txInfoSignatories info

    checkDeadline :: Bool
    checkDeadline = from s `contains` txInfoValidRange info
```

For the inst implementation, we should not forget our datum (@Slot)

```haskell
inst :: PubKeyHash -> Scripts.ScriptInstance Vesting
inst p = Scripts.validator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
where
    wrap = Scripts.wrapValidator @Slot @()
```

The Validator implementation is exactly the same as in Parameterized.hs

```haskell
validator :: PubKeyHash -> Validator
validator = Scripts.validatorScript . inst
```

Same goes for our scrAddress

```haskell
scrAddress :: PubKeyHash -> Ledger.Address
scrAddress = scriptAddress . validator
```

The rest of the wallet code is already prepared for us.

## Footnote

Great! Week03 Done!

On to the next one.

Happy Coding! 😊

## Credits

Condensed version of Lecture #3 of the Plutus Pioneer Program by Lars Brünjes on [Youtube](https://www.youtube.com/watch?v=Lk1eIVm_ZTQ)
Cloned from [Reddit (u/RikAlexander)](https://www.reddit.com/r/cardano/comments/n3evg4/week_03_plutus_pioneer_program/)
