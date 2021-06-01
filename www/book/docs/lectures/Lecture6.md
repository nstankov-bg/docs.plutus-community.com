# Lecture #6 - Week06 Plutus Pioneer Program

## Credits

Condensed version of Lecture #6 of the Plutus Pioneer Program by Lars Brünjes on [Youtube](https://www.youtube.com/watch?v=wY7R-PJn66g)

Cloned from [Reddit (u/RikAlexander)](https://www.reddit.com/user/RikAlexander)

[Reddit Part 1](https://www.reddit.com/r/cardano/comments/nls3of/week_06_plutus_pioneer_program_part_1/)
[Reddit Part 2](https://www.reddit.com/r/cardano/comments/nls6mh/week_06_plutus_pioneer_program_part_2/)

## Overview

This lecture is all about Oracles!

We'll be creating a fully fledged Dapp, running on `mockchain` since we don't have a real blockchain available yet.

It will contain all the pieces we need for real applications, as soon as Plutus is available on the Cardano Main Chain.

## Oracle

What is an Oracle?

An Oracle is a Service/Way to get real world data onto the blockchain, and making it usable in smart contracts.

Examples of real world data could be:

- Weather data
- Election results
- Stock exchange rates
- Randomness for e.g. games
- Time
- Temperatures
- Water Levels
- Betting on the outcome of a sports game

Etc.

## Example Oracle

In the example given by Lars, we'll be looking at creating an Oracle that retrieves the exchange rate from ADA to USD.

Of course it is important that we use a source that we trust.

Possible problems we need to tackle:

- Request may fail to provide correct data
- Request may fail to provide data at all

One way would be to make the provider put down collateral; if it then fails to provide data to us, it looses this collateral.

Another way would be to combine multiple oracles, if all of them agree on the data (e.g. ADA-USD exchange rate) we'd trust this data; or we could take the average of all of them.

For this example though, we'll just take one provider that we trust.

## How

For anything to happen on the blockchain, we need an UTxO. So our Oracle value (`Datum`) will sit a the script address of the Oracle.

This is our first obstacle, as Validation only happens when you consume something from a script address, not when you produce an output at a script.

Which means that we can't prevent anybody from providing arbitrary outputs at the same Oracle script address.

We'll need a way to distinguish true UTxO Oracle output from the others sitting at the same script address.

For this we can use NFTs (which we learned about in [week 05](https://www.reddit.com/r/cardano/comments/ne0nuz/week_05_plutus_pioneer_program/)).

NFTs are always unique, there is only one of them on the whole blockchain.

So to make the correct Oracle output unique, is to not only have it carry the data, but also an NFT. Because of the uniqueness of the NFT, it will allow us to uniquely identify the correct UTxO that carries the correct Oracle value.

Since we don't know how this Oracle will be used, we'll create it as a sort of `Open API`. I.e. it must be usable by smart contracts that do not exist at the time the Oracle is created.

## ADA - USD (Swap Contract)

For the oracle to provide the data, it will cost a specific fee.

So for our ADA -> USD script, if the buyer would like to buy 100 ADA with USD (the Seller), the buyer would need to pay the correct exchanged USD amount, and a fee for the oracle usage.

We'll be creating multiple validators, one of which we'll call `use`.

It needs to check multiple things:

- the NFT must be present in the UTxO
- make sure there is an output at the same oracle address
- this output must also contain the NFT and the same Oracle value
- the Fee must be payed

Another validator we'll create is the `swap` validator. As the name suggests, it will do the actual swap of ADA -> USD.

It consumes the UTxO of the `Seller` (containing the ADA), the `Buyer` UTxO with the USD and the fees for the Oracle, and will then (if everything checks out) give the USD to the `Seller`, and the ADA to the `Buyer`.

## Update Oracle Value

Now especially for Exchange rates of course, they will change over time.

Thus we need another function to `update` the current Oracle Value.

Important to note here: UTxOs can never "change", we can only consume them, and create new ones; which is what we'll be doing to update our Oracle Value (exchange rate from ADA - USD in this case).

The transaction doing the update, must consume the UTxO and provide a new one with the updated value and it must be signed by the Oracle provider (we wouldn't want just anybody changing the Oracle value) and it should also hold the same NFT of course.

This also gives us the possibility to collect all the fees that were payed by users of this Oracle.

## Oracle.core

Open the file `Core.hs` in plutus-pioneer-program/code/week06/src/Week06/Core.hs

Lines 47 - 52

```haskell
data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol
    , oOperator :: !PubKeyHash
    , oFee      :: !Integer
    , oAsset    :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
```

`oSymbol` -> the CurrencySymbol of our NFT. We don't need the TokenName, as we'll just leave this empty.

`oOperator` -> the public key hash of the owner/operator of the Oracle (can do updates, and collect the fees)

`oFee` -> the fees in lovelaces for the usage of the oracle data

`oAsset` -> what to exchange for our ADA (in this case this would be USD (represented as a Token, since USD of course does not exist on the Blockchain))

Line 56

```haskell
data OracleRedeemer = Update | Use
```

We want to support our to functions `update` and `use`, so our `OracleRedeemer` can be either.

---------

For all of our exchange rates, although Plutus has a `Ratio` type, we'll be using `Integer`, `Ratio` has some issues.

Using `Integer` as a 1mio unit, we don't have these issues. (e.g. 1.75 = 1 750 000).

## mkOracleValidator

This is where all of the magic happens.

Line 77

```haskell
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
```

`Oracle` -> parametrization of our Oracle Data type

`Integer` -> Our `Datum`, in this case the current exchange rate

`OracleRedeemer` -> `use` or `update`

`ScriptContext` -> should be clear by now (if not check out [week 05](https://www.reddit.com/r/cardano/comments/ne0nuz/week_05_plutus_pioneer_program/))

Lines 79 - 80

```haskell
traceIfFalse "token missing from input"  inputHasToken  &&
traceIfFalse "token missing from output" outputHasToken &&
```

For both `use` or `update`, these 2 checks are the same, which is why we do them upfront.

First we check that we have input that holds the NFT (`inputHasToken`), and the same for our output (`outputHasToken`)

---------

### Input

Lines 90 - 93

```haskell
ownInput :: TxOut
ownInput = case findOwnInput ctx of
    Nothing -> traceError "oracle input missing"
    Just i  -> txInInfoResolved i
```

Retrieves the Oracle input using `findOwnInput` on our `ScriptContext`, or returns an error if none is found.

Lines 95 - 96

```haskell
inputHasToken :: Bool
inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1
```

Checks if the NFT exists exactly once in the UTxO input (`ownInput`), using `assetClassValueOf` which returns the amount of times the asset is present.

---------

### Output

Lines 98 - 101

```haskell
ownOutput :: TxOut
ownOutput = case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "expected exactly one oracle output"
```

Using `getContinuingOutputs` we get a list of all the outputs that go to the same script address that we are currently validating.

Since we expect/want only 1 output, using `[o]` we imply that only when there is 1 item in the list, we return it, otherwise `_` we throw an error.

Lines 95 - 96

```haskell
outputHasToken :: Bool
outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1
```

Checks if the NFT exists exactly once in the UTxO output (`ownOutput`).

---------

Basically all we did was check if both the input and the output has the NFT.

Next is the individual validation for both `update` and `use`.

Lines 81 - 85

```haskell
case r of
    Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&
                traceIfFalse "invalid output datum"       validOutputDatum
    Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&
                traceIfFalse "fees not paid"              feesPaid
```

For `update`, we check if the operator signed the transaction by using `txSignedBy`.

And if the output datum (in this case our Exchange rate) is valid by using `validOutputDatum` defined on lines 106 - 110

```haskell
outputDatum :: Maybe Integer
outputDatum = oracleValue ownOutput (`findDatum` info)

validOutputDatum :: Bool
validOutputDatum = isJust outputDatum
```

For the `update` part of the script, we are only interested in checking if it is a `Integer`, for the `use` function though, we also want to check if the Datum HASN'T changed.

The `use` function can be called by anybody, and thus is much more restrictive in that it of course does not allow for any changes to the Datum (exchange rate).

Which is what we check with `outputDatum == Just x` (where x is our input datum).

Last but not least, when calling the `use` function, fees must be paid to the Oracle for providing the data.

We check if they are paid with `feesPaid`, defined on lines 112 - 118

```haskell
feesPaid :: Bool
feesPaid =
    let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
    in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))
```

Checks if the outputs are greater than or equal to the inputs + fees.

By doing `geq` (Greater than or equal), we allow for the user of the Oracle to pay more than necessary thus allowing a tip!

Lines 120 - 142 are just boilerplate code, to make everything work for the on-chain part. This is all explained in the previous weeks, so I won't be going over them here.

## startOracle

Lines 144 - 156

```haskell
startOracle :: forall w s. HasBlockchainActions s => OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    osc <- mapError (pack . show) (forgeContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle
```

`startOracle`, does what it says it'll do :D

Here we are forging the NFT which will be used throughout all functions.

We could of course do this with the code we wrote in [week 05](https://www.reddit.com/r/cardano/comments/ne0nuz/week_05_plutus_pioneer_program/)

But this time we'll use the `forgeContract` function, with our Contract Public Key to forge our NFT.

`mapError` is used here, because we want to map the Contract monads error messages to `Test`, instead of in this case CurrencyError.

Lines 158 - 174

```haskell
updateOracle :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (oracleInst oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.scriptInstanceLookups (oracleInst oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x
```

`updateOracle` needs to handle the case when there is no UTxO yet, we do so by calling `findOracle`

```haskell
findOracle :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, x)
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1
```

Basically checks if at the script address a UTxO with NFT exists, if not return `Nothing`.

`updateOracle` then continues to either create the UTxO at the address with the first exchange rate, (`let c`) `mustPayToTheScript` with the NFT and `Datum`.

So we create a new Transaction (`submitTxConstraints`), and log that we have set the initial oracle value.

If it does exist already, the existing UTxO must be spend (because we cant "change" an existing UTxO, we can only spend the existing one, and create a new one with the updated value).

We'll also create a new transaction here, only with one more constraint.

The first `Constraint` is the same as before (`let c`) `mustPayToTheScript`, because we want to create a new UTxO, only this time we also want to spend the current Existing UTxO, which we'll do by adding the `mustSpendScriptOutput` constraint.

To find the current UTxO we use the lookup `unspentOutputs`, which together with `Map.singleton` returns just one output.

Now since we have an `input` of the NFT AND the Fees that we've collected from the users of the Oracle.

But our new Output UTxO only contains the NFT.

When we call the `mustSpendScriptOutput`, it will create an imbalance, the balancing algorithm will automatically balance the transaction, and transfer all user paid fees to the wallet of the operator.

## Run Oracle

`runOracle` defined on lines 188 - 200

```haskell
type OracleSchema = BlockchainActions .\/ Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle
```

Creates the endpoint `update`, starts the oracle (which as why explained earlier, mints the NFT), and writes the Oracle value (our exchange rate).

We then use `tell` to communicate our just created Oracle to the outside world.

Since `tell` requires a `monoid`, we use `Last`.

`Last` is a `monoid` operation that remembers our last `Just` value.

Small snippet which explains `Last` really nicely.

```haskell
Last (Just 'x') <> Last (Just 'y')
    > Last { getLast = Just 'y' }

Last (Just 'x') <> Last Nothing
    > Last { getLast = Just 'x' }

Nothing <> Last Nothing
    > Last { getLast = Nothing }
```

At the end it'll call `go oracle` again. (infinite loop).

## Swap contract

The `Swap` contract will be using our Oracle.

Open the file `Swap.hs` plutus-pioneer-program/code/week06/src/Week06/Swap.hs

We will put ADA in the smart contract, which somebody can then exchange for another token (USD) (Since there is no USD on the blockchain, we will use USDT as a USD Token).

The exchange rate will be determined by the Oracle, which may change over time (obviously).

We'll create the validator `mkSwapValidator`.

Lines 46 - 51

```haskell
{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =
    txSignedBy info pkh ||
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
     traceIfFalse "price not paid"                     sellerPaid)
```

`Oracle` -> from the `Oracle` module

`Address` -> the Oracle Address.

*Note:* Normally we could compute the oracle address with a function on our first parameter `Oracle`, but this is not doable in this Validator, it can't be compiled into Plutus, so we have to explicitly give our Validator the Oracle Address.

Our `Datum` is the `PubKeyHash` of the Seller.

And for the `Redeemer` we have a value of type Unit (Void).

Now first we check if the Transaction is signed by the seller. If so: the next two checks are obsolete.

Otherwise, we'd first check if we have exactly 2 script inputs (thus avoiding interference with other smart contracts). Defined on lines 74 - 79

```haskell
hasTwoScriptInputs :: Bool
hasTwoScriptInputs =
    let
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
    in
        length xs == 2
```

Filter for script inputs and check if there are exactly 2.

We'd also check if the seller gets paid. Lines 90 - 96

```haskell
sellerPaid :: Bool
sellerPaid =
    let
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
    in
        pricePaid >= minPrice
```

We sum up all outputs that go to the public key address with `valuePaidTo info pkh`.

Then we compare this to `minPrice`, which is defined on lines 81 - 88

```haskell
minPrice :: Integer
minPrice =
    let
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
    in
        price lovelaceIn oracleValue'
```

Which gets how may lovelaces are locked in the Swap.

Also in our `where`, the first few lines (57 - 72) gets the Oracle Input (the exchange rate).

```haskell
oracleInput :: TxOut
oracleInput =
    let
        ins = [ o
            | i <- txInfoInputs info
            , let o = txInInfoResolved i
            , txOutAddress o == addr
            ]
    in
        case ins of
            [o] -> o
            _   -> traceError "expected exactly one oracle input"

oracleValue' = case oracleValue oracleInput (`findDatum` info) of
    Nothing -> traceError "oracle value not found"
    Just x  -> x
```

With some error handling when we get more than one input, or no value at all.

------------

### Offer Swap Contract

Defined on lines 118 - 124

```haskell
offerSwap :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
    ledgerTx <- submitTxConstraints (swapInst oracle) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"
```

The offer swap contract is for the seller if he wants to provide a swap (ADA -> USD(T))

It takes as parameters the `Oracle`, and an Integer, which is the amount of lovelaces the seller wants so put up for swapping.

Basically all this contract does, is Pay lovelaces to the script.

------------

### Helper function: findSwaps

Defined on lines 126 - 141

```haskell
findSwaps :: HasBlockchainActions s => Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
findSwaps oracle p = do
    utxos <- utxoAt $ swapAddress oracle
    return $ mapMaybe g $ Map.toList utxos
  where
    f :: TxOutTx -> Maybe PubKeyHash
    f o = do
        dh        <- txOutDatumHash $ txOutTxOut o
        (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o
        PlutusTx.fromData d

    g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash)
    g (oref, o) = do
        pkh <- f o
        guard $ p pkh
        return (oref, o, pkh)
```

This function will return all the swaps, that fulfill a predicate `(PubKeyHash -> Bool)`. (this will be used in the next couple functions, for getting the correct UTxOs)

`mapMaybe` is defined as `mapMaybe :: (a -> Maybe b) -> [a] -> [b]`, this function basically filters a list based if the value is `Just` or `Nothing`.

An example:

```haskell
f (n :: Int) = if even n then Just (div n 2) else Nothing
```

`f` is a function that takes a Integer, which on an even number, will return a `Just` value of that Integer divided by 2, otherwise it will return `Nothing`.

Combined with `mapMaybe`, we could run it as

```haskell
mapMaybe f [2, 4, 10, 11, 13, 100]
```

Which would then return `[1, 2, 5, 50]`.

So in this case it filters the UTxOs at the `swapAddress` of the oracle, with `f` returning the PubKeyHash (or failing if we cant get it), and `g` then running it against our predicate. We are using `guard` for this, if the predicate returns `True`, guard won't do anything, but when it's `False`, `guard` will stop right there (i.e. guarding for a `False` statement).

### retrieveSwaps contract

Defined on lines 143 - 155

```haskell
retrieveSwaps :: HasBlockchainActions s => Oracle -> Contract w s Text ()
retrieveSwaps oracle = do
    pkh <- pubKeyHash <$> ownPubKey
    xs <- findSwaps oracle (== pkh)
    case xs of
        [] -> logInfo @String "no swaps found"
        _  -> do
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                          Constraints.otherScript (swapValidator oracle)
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | (oref, _, _) <- xs]
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"
```

`retrieveSwaps` will return all the Swaps that belong to us.

We use the `findSwaps` function that we've defined earlier; with the predicate `(== pkh)`, in other words, return all the swaps where the `pubKeyHash` equals our `ownPubKey`.

If `findSwaps` returns none, just log "no suitable swap found", otherwise, create a transaction that returns all of our swaps to us.

### useSwap contract

Defined on lines 157 - 193

```haskell
useSwap :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text ()
useSwap oracle = do
    funds <- ownFunds
    let amt = assetClassValueOf funds $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle
    case m of
        Nothing           -> logInfo @String "oracle not found"
        Just (oref, o, x) -> do
            logInfo @String $ "found oracle, exchange rate " ++ show x
            pkh   <- pubKeyHash <$> Contract.ownPubKey
            swaps <- findSwaps oracle (/= pkh)
            case find (f amt x) swaps of
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do
                    let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle)
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                  Constraints.otherScript (oracleValidator oracle)                   <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toData Use) <>
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toData ())  <>
                                  Constraints.mustPayToOtherScript
                                    (validatorHash $ oracleValidator oracle)
                                    (Datum $ PlutusTx.toData x)
                                    v                                                                      <>
                                  Constraints.mustPayToPubKey pkh' p
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> TxOutTx -> Integer
    getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x

    f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool
    f amt x (_, o, _) = getPrice x o <= amt
```

This is the interesting contract, as this one will make use of the Oracle.

We start with the `ownFunds` function (we'll define this function later on in the `Funds module`), that just adds up all funds of the one calling `useSwap`, which we then filter with `assetClassValueOf` to return all of our `oAsset oracle` (in this case the USD Token).

We then try to find the Oracle (the UTxO that contains the Oracle and the Value (Exchange rate)) with `findOracle` (defined earlier).

We then search for all swaps that we are NOT the owner of (predicate `/= pkh`).

And try to find one that we can afford (one that we have enough funds for to swap with) with `find (f amt x) swaps`.

In real life of course this wouldn't make much sense. You'd probably want to specify a specific amount that you would want to swap, etc. but since this lecture is about the oracles and not a realistic swap contract, we've kept it simple here.

If we do find one we can afford, just take the first one we find.

We'll then construct a transaction to do the swap.

The output of the oracle, will be the existing output, plus the fees we need to pay added to it with `<> lovelaceValueOf (oFee oracle)`.

We then define `p` which will be the amount we have to pay to do the actual swap by taking the lovelaces contained in the Swap (`o'`) and the exchange rate from the Oracle, and multiply it by using our `price` function that we defined earlier.

We create some constraints for the transaction:

1. We'll spend the Oracle output with the `Use` redeemer (first time using it here, instead of the `Update` redeemer)

2. We consume/spend the swap input

3. We need to pay the Fee to the Oracle

4. We need to pay the seller of the lovelace (with the price we calculated `p`)

To make everything work, we need to also add lookups for the `swapValidator`, the `oracleValidator`, and the two UTxOs we want to consume (the oracle / the swap)

### Endpoints

Lines 195 - 200

```haskell
type SwapSchema =
    BlockchainActions
        .\/ Endpoint "offer"    Integer
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      ()
        .\/ Endpoint "funds"    ()
```

`offer` -> to offer a swap (with Integer as the amount of ADA we want to offer for swapping)

`retrieve` -> to retrieve all offered swaps

`use` -> to do a swap

`funds` -> will give back my currently available funds

To combine all endpoints, thus calling the correct Contract associated with it.

We'll use `select`. (Line 203)

```haskell
swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle
```

Basically offers all endpoints, and executes the first one that is triggered, then recursively calls itself to start all over again.

Lines 205 - 224, will bind all endpoints to the correct contract.

```haskell
offer :: Contract (Last Value) SwapSchema Text ()
offer = h $ do
    amt <- endpoint @"offer"
    offerSwap oracle amt

retrieve :: Contract (Last Value) SwapSchema Text ()
retrieve = h $ do
    endpoint @"retrieve"
    retrieveSwaps oracle

use :: Contract (Last Value) SwapSchema Text ()
use = h $ do
    endpoint @"use"
    useSwap oracle

funds :: Contract (Last Value) SwapSchema Text ()
funds = h $ do
    endpoint @"funds"
    v <- ownFunds
    tell $ Last $ Just v
```

We wraps all contracts with the error handler `h` (Lines 226 - 227), that will just log the error, and continue.

```haskell
h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
h = handleError logError
```

## Funds Module

We've used the function `ownFunds` a few times now, basically all this does is return all the funds I have in my wallet.

The `ownFunds` function is defined in the Funds module (Funds.hs).

Lines 29 - 35

```haskell
ownFunds :: HasBlockchainActions s => Contract w s Text Value
ownFunds = do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
    return v
```

Gets our own public key, all the UTxOs at that key, and adds all the values of all UTxOs together with mconcat, after which we'll log this, and return the total of funds in this wallet.

We also have a small variation on this function, namely `ownFunds'`, which uses the original `ownFunds` function, only `tell`'s the value instead of returning it.

Lines 37 - 41

```haskell
ownFunds' :: Contract (Last Value) BlockchainActions Text ()
ownFunds' = do
    handleError logError $ ownFunds >>= tell . Last . Just
    void $ Contract.waitNSlots 1
    ownFunds'
```

## Test module

All our code is ready. Ready to test! (the fun part)

File: `Test.hs`

We'll start by defining our base asset (our `USD` token)

Lines 36 - 40

```haskell
assetSymbol :: CurrencySymbol
assetSymbol = "ff"

assetToken :: TokenName
assetToken = "USDT"
```

Of course the CurrencySymbol would normally be the Hash of a real Currency, but for testing purposes `ff` will suffice.

Next we use the extended `runEmulatorTraceIO` -> `runEmulatorTraceIO'` (Line 43), which will give us more customization properties, specifically the ability to provide wallets with an initial distribution of Tokens.

Lines 45 - 50

```haskell
emCfg :: EmulatorConfig
emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 10]]

v :: Value
v = Ada.lovelaceValueOf                    100_000_000 <>
    Value.singleton assetSymbol assetToken 100_000_000
```

We'll create 10 wallets, with each 100 lovelaces and 100 of our `USDT` tokens.

Next a helper contract `checkOracle`, that will periodically (every 1 slot) check the oracle value (exchange rate), and log it. (this is of course very helpful for our test).

Lines 52 - 58

```haskell
checkOracle :: Oracle -> Contract () BlockchainActions Text a
checkOracle oracle = do
    m <- findOracle oracle
    case m of
        Nothing        -> return ()
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x
    Contract.waitNSlots 1 >> checkOracle oracle
```

Next we will define our trace! Lines 60 - 111

```haskell
myTrace :: EmulatorTrace ()
myTrace = do
    let op = OracleParams
                { opFees = 1_000_000
                , opSymbol = assetSymbol
                , opToken  = assetToken
                }

    h1 <- activateContractWallet (Wallet 1) $ runOracle op
    void $ Emulator.waitNSlots 1
    oracle <- getOracle h1

    void $ activateContractWallet (Wallet 2) $ checkOracle oracle

    callEndpoint @"update" h1 1_500_000
    void $ Emulator.waitNSlots 3

    void $ activateContractWallet (Wallet 1) ownFunds'
    void $ activateContractWallet (Wallet 3) ownFunds'
    void $ activateContractWallet (Wallet 4) ownFunds'
    void $ activateContractWallet (Wallet 5) ownFunds'

    h3 <- activateContractWallet (Wallet 3) $ swap oracle
    h4 <- activateContractWallet (Wallet 4) $ swap oracle
    h5 <- activateContractWallet (Wallet 5) $ swap oracle

    callEndpoint @"offer" h3 10_000_000
    callEndpoint @"offer" h4 20_000_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_700_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_800_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"retrieve" h3 ()
    callEndpoint @"retrieve" h4 ()
    void $ Emulator.waitNSlots 3
  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        l <- observableState h
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle
```

1. Start the oracle `runOracle op` with the correct params (amount of fees / our USDT)

2. use the `getOracle` helper function, which checks with `observableState` if the oracle is there, if not, wait for 1 slot and try again.

3. Initialize the Oracle with the `update` endpoint with a value (exchange rate) of 1.5 USDT per ADA.

4. `ownFunds'` for telling our initial funds

5. `swap oracle` to start the swap contract on wallet 3, 4 and 5

Now we're ready for some test scenarios.

Wallet 3 and 4 will offer ADA for swapping (10 and 20).

Wallet 5 uses the swap with the endpoint `use`.

*Note:* we don't know which swap will be used, since we've defined the `useSwap` function to just return the first swap we have the funds for.

Now we'll update the exchange rate to 1.7 USDT for each ADA (with the `update` endpoint).

And we'll have Wallet 5 use the swap again (endpoint `use`).

Now we will update the exchange rate to 1.8 USDT, which will automatically have wallet 1 retrieve all the fees paid to the oracle.

------------

If we now run our Trace, the final balances SHOULD look like this:

`Wallet 1` -> close to 2 ADA more than before (wallet 1 paid fees to start the oracle)

`Wallet 2` -> nothing changed (we didn't do anything with wallet 2)

`Wallet 3` -> got USD(T) for an exchange rate of 1.7, paid the corresponding ADA and Oracle usage fees

`Wallet 4` -> got USD(T) for an exchange rate of 1.8, paid the corresponding ADA and Oracle usage fees

`Wallet 5` -> Paid the USD(T) to wallet 3 and 4, got ADA in return of course.

## Plutus Application Backend (PAB)

We will be creating an executable which runs our contracts!

If the testnet was available we could deploy it on there, but for now we have to use the Mockchain.

File: `PAB.hs`

This is a very small module, required because we will use this module both in the PAB and on the frontend.

Lines 15 - 16

```haskell
data OracleContracts = Init | Oracle CurrencySymbol | Swap Oracle.Oracle
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
```

We have to define a data type for all contracts:

`Init` -> environment setup (e.g. initial funds in `Emulatortrace`)

`Oracle CurrencySymbol` -> the `runOracle` contract (with the `update` function to update the oracle value)

`Swap Oracle.Oracle` -> will run the `swap` contract including all of it's endpoints

Now in the Cabal file (`plutus-pioneer-program-week06.cabal` in the root of the week06 directory)

We'll create 3 executables:

### `oracle-pab`

- Will start the simulated wallet

- Initialize all contracts

- Set-up a webserver that will allow us to interact with the contracts

### `oracle-client`

- Will be for the Oracle provider to fetch the Exchange rates from the internet to be feeded into the Oracle

- Runs the Oracle Contract

### `swap-client`

- For clients that want to make use of the swap contract


------------

Now we'll run over these 3 executables, all of them are located in the `/app` directory of week06.

## Oracle PAB

File: `/app/oracle-pab.hs`

First `handleOracleContracts` which is all boilerplate code.

If we look at `OracleContracts` data type, `handleOracleContracts` connects each of these (`Init`, `Oracle`, `Swap`) to Contracts and Schemas.

Lines 91 - 99

```haskell
handleOracleContracts = handleBuiltin getSchema getContract where
    getSchema = \case
        Init     -> endpointsToSchemas @Empty
        Oracle _ -> endpointsToSchemas @(Oracle.OracleSchema .\\ BlockchainActions)
        Swap _   -> endpointsToSchemas @(Oracle.SwapSchema   .\\ BlockchainActions)
    getContract = \case
        Init        -> SomeBuiltin   initContract
        Oracle cs   -> SomeBuiltin $ Oracle.runOracle $ oracleParams cs
        Swap oracle -> SomeBuiltin $ Oracle.swap oracle
```

`Init`-> will be running the `initContract` defined in this file as well. (Lines 106 - 123)

`Oracle cs` -> will run `Oracle.runOracle` with `cs` as the `oracleParams` (1 ADA fees, currencySymbol `cs` and the Token usdt) (Lines 78 - 83)

```haskell
oracleParams :: CurrencySymbol -> Oracle.OracleParams
oracleParams cs = Oracle.OracleParams
    { Oracle.opFees   = 1_000_000
    , Oracle.opSymbol = cs
    , Oracle.opToken  = usdt
    }
```

`Swap` -> with a given oracle parameter, will just run `Oracle.swap` with oracle as parameter

The `initContract` contract, is basically only there for the demo; it will provide all wallets with initial funds. Gives every wallet 100 usdt tokens, for a total of 5 wallets (hardcoded amount of wallets on line 73 `wallets = [Wallet i | i <- [1 .. 5]]`).

### Main

Main is the actual PAB code.

Defined on lines 45 - 64

```haskell
main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin OracleContracts) "Starting Oracle PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit <- Simulator.activateContract (Wallet 1) Init
    cs      <- waitForLast cidInit
    _       <- Simulator.waitUntilFinished cidInit

    cidOracle <- Simulator.activateContract (Wallet 1) $ Oracle cs
    liftIO $ writeFile "oracle.cid" $ show $ unContractInstanceId cidOracle
    oracle <- waitForLast cidOracle

    forM_ wallets $ \w ->
        when (w /= Wallet 1) $ do
            cid <- Simulator.activateContract w $ Swap oracle
            liftIO $ writeFile ('W' : show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid

    void $ liftIO getLine
    shutdown
```

Main will be using the `Simulator` monad, which is similar to the `EmulatorTrace` monad that we've now looked into multiple times; you can start contracts on wallets, inspect the log, the state, and you can call endpoints.

They are so alike, that apparently the Plutus team is thinking about combining these two monads into one; but we'll just have to wait and see until further notice.

One big difference though, with the `EmulatorTrace` monad there was only pure code, no side-effects or IO involved whatsoever. The `Simulator` monad on the other hand, by using `MonadIO` and `liftIO`, any basic haskell `IO` action can be "lifted" into the monad in question.

We can log information with `Simulator.logString`.

`PAB.Server.startServerDebug` will start the server and return a function to later shutdown the server again.

Then we'll run `Simulator.activateContract` on wallet 1, which is very similar to our `EmulatorTrace activateContractWallet` function, with the function `Init` (in `handleOracleContracts`). Which will run our InitContract function that just gives all wallets 100 usdt.

Now the minting of our usdt tokens will take some time, so we'll have to wait for this to be ready.

To do this we have the helper function `waitForLast`, which does exactly that. (Lines 66 - 70)

```haskell
waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing
```

It waits until the contract `tell`'s a `Last` state. Which when `Nothing` would fail.

Now the state is serialized as JSON, which must be parsed, this of course could also fail, and is handled by the `Success` function provided by the [Aeson module](https://hackage.haskell.org/package/aeson).

Next we will start the Oracle on wallet 1 (with the CurrencySymbol `cs`) (`Simulator.activateContract (Wallet 1) $ Oracle cs`)

This will return a handle (UUID) to the Oracle (`cidOracle`) which we will later need to communicate with it (through the web interface for example), so we'll write this to the file `oracle.cid` with the `writeFile` IO function (by Lifting it `liftIO`).

*Note:* this is just "quick & dirty", in production code, we would probably do this in some more sophisticated way.

Last but not least, we'll start the Swap contract for each wallet (except wallet 1 which runs the oracle `/= Wallet 1`).

This will also return a cid/UUID, which we also need for the Web Interface later on, so loop through wallets 2-5 and write the cids of the swap contracts to files as well (e.g. `W2.cid`).

Now in principle we don't need to do all this; all of these functions can be done through the REST API provided by Plutus in (`plutus-pab/src/Plutus/PAB/Webserver/API.hs`), but it's easier to do it programmatically.

`NewAPI` (starts on line 42 in `API.hs`)
    - `/api/new/contract`
    - `/activate`
    - `/instances`
    - `/instance/$cid/(status/endpoint/stop)`
    - `/definitions`

*Note:* There is also a Websocket API provided (`WSAPI`)

We can now run our `oracle-pab` with `cabal run oracle-pub`, which will start the live server, and if we look in the file explorer we can see all the created CID files.

## Oracle Client

Now, all of these next lines are done through the REST endpoints, and thus could be done in any language (Java, PHP, etc. etc.), here we did in in Haskell.

File: `app/oracle-client.hs`

Since most of this is self explanatory, and as I said could be written in any language, I won't be going over it in too much detail, nevertheless here is the code for reference:

```haskell
main :: IO ()
main = do
    uuid <- read <$> readFile "oracle.cid"
    putStrLn $ "oracle contract instance id: " ++ show uuid
    go uuid Nothing
  where
    go :: UUID -> Maybe Integer -> IO a
    go uuid m = do
        x <- getExchangeRate
        let y = Just x
        when (m /= y) $
            updateOracle uuid x
        threadDelay 5_000_000
        go uuid y

updateOracle :: UUID -> Integer -> IO ()
updateOracle uuid x = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "update")
        (ReqBodyJson x)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "updated oracle to " ++ show x
        else "error updating oracle"

getExchangeRate :: IO Integer
getExchangeRate = runReq defaultHttpConfig $ do
    v <- req
        GET
        (https "coinmarketcap.com" /: "currencies" /: "cardano")
        NoReqBody
        bsResponse
        mempty
    let priceRegex      = "priceValue___11gHJ\">\\$([\\.0-9]*)" :: ByteString
        (_, _, _, [bs]) = responseBody v =~ priceRegex :: (ByteString, ByteString, ByteString, [ByteString])
        d               = read $ unpack bs :: Double
        x               = round $ 1_000_000 * d
    liftIO $ putStrLn $ "queried exchange rate: " ++ show d
    return x
```

In the Main function, we read the `oracle.cid` file (which we need for API usage).

Get the exchange rate (`getExchangeRate`) every 5 seconds.

If it has changed (`m /= y`), we update the contract (`updateOracle`).

`updateOracle` in essence just does a `POST` request to `/api/new/contract/instance/$cid/endpoint/update` with the new exchange rate as an Integer in JSON.

HTTP Status 200 (OK) we'd log it, same for a different Status.

`getExchangeRate`, gets the exchange rate from coinmarketcap.

HTTP GET Request to `https://coinmarketcap.com/currencies/cardano`, which is just the HTML homepage for Cardano on Coinmarketcap.

![Coin Market Cap Homepage](https://preview.redd.it/nqimh4rwaj171.png?width=287&format=png&auto=webp&s=a023bc687955bb9dfe4e27809c2c6cd8b75a8e35)

On which we will then extract the USD exchange rate from the HTML with a quick&dirty regex. `"priceValue___11gHJ\">\\$([\\.0-9]*)"`.

![Coin Market Cap HTML](https://preview.redd.it/159uzsqxaj171.png?width=405&format=png&auto=webp&s=8d69d4aa2d7813b2bee287d688dcc743a1102f62)

*Note:* in production of course we would need some real API to get our information from.

## Swap Client

This is the client where we can do the Swap!

For now this is just a CLI client, this could of course also be done through a nice web UI.

File: `app/swap-client.hs`

The Main IO function expects the Wallet number as argument, to load the correct CID/UUID from the file(s) `W2-5.cid`.

Lines 27 - 52

```haskell
main :: IO ()
main = do
    [i :: Int] <- map read <$> getArgs
    uuid       <- read <$> readFile ('W' : show i ++ ".cid")
    hSetBuffering stdout NoBuffering
    putStrLn $ "swap contract instance id for Wallet " ++ show i ++ ": " ++ show uuid
    go uuid
  where
    go :: UUID -> IO a
    go uuid = do
        cmd <- readCommand
        case cmd of
            Offer amt -> offer uuid amt
            Retrieve  -> retrieve uuid
            Use       -> use uuid
            Funds     -> getFunds uuid
        go uuid

    readCommand :: IO Command
    readCommand = do
        putStr "enter command (Offer amt, Retrieve, Use or Funds): "
        s <- getLine
        maybe readCommand return $ readMaybe s

data Command = Offer Integer | Retrieve | Use | Funds
    deriving (Show, Read, Eq, Ord)
```

All it really does is provide a "ui" for a specific wallet, with the functions "Offer", "Retrieve", "Use" and "Funds".

All of which will run the corresponding IO function. (lines 54 - 121).

All these functions do is execute requests to the correct endpoints, parse its results, and log its output.

`Funds` -> POST /api/new/contract/instance/UUID/endpoint/funds

`Offer` -> POST /api/new/contract/instance/UUID/endpoint/offer

`Retrieve` -> POST /api/new/contract/instance/UUID/endpoint/retrieve

`Use` -> POST /api/new/contract/instance/UUID/endpoint/use

If we want to run this for wallet 2 for example:

```bash
[$] cabal run swap-client -- 2
```

We could then test it by e.g.

- Open 2 terminal windows, one for wallet 2 and one for wallet 3 (with the above command)

- Wallet 2 - Offering 10 ADA up for swaps `Offer 1000000`
- Wallet 2 - Check the funds: `Funds`

- Wallet 3 - Swap: `Use`
- Wallet 3 - Check the funds: `Funds`

We should now see that Wallet 3: made swap, payed the Oracle fees, and payed the transaction fees.

Whereas wallet 2 has: received USD(t) with the live exchange rate used to compute the correct amount.

*Note:* since we are working with the Mockchain (simulated blockchain), we have 1 PAB for all contracts.

In the "real world" (or testnet for that matter), we would have multiple PAB's for different contracts.

## Footnote

Week06 Done!

Happy Coding! 😊

## Credits

Condensed version of Lecture #6 of the Plutus Pioneer Program by Lars Brünjes on [Youtube](https://www.youtube.com/watch?v=wY7R-PJn66g)

Cloned from [Reddit (u/RikAlexander)](https://www.reddit.com/user/RikAlexander)

[Reddit Part 1](https://www.reddit.com/r/cardano/comments/nls3of/week_06_plutus_pioneer_program_part_1/)
[Reddit Part 2](https://www.reddit.com/r/cardano/comments/nls6mh/week_06_plutus_pioneer_program_part_2/)
