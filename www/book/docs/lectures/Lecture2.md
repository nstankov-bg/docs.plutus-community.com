# Week02

### Credits

[Alberto Calzada - albertoCCz](https://github.com/albertoCCz)

## Validator
The _Validator_ is a function that takes three arguments, _Datum_, _Redeemer_ and _Context_, which are of type _Data_, and returns the _Unit_ type (which reads `()` on Haskell) or a _Bool_ type (maybe others, although I think it would make no sense). As an example:

```haskell
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()
```

This Validator will always accept any transaction, because no matter what arguments it takes (that's why we use the `_`) that the output will be of Unit type, so the transaction will succeed. You can, of course, change the logic of the validator so it matches your business requirements. As an example:

```haskell
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ r _
	| r == I 42 = ()
    	| otherwise = traceError "wrong redeemer"
```

(This won't probably match anyone's business requirements, but it is bit more complex than the previous one ;))

As we can see, we have implemented some simple logic for the redeemer. Now, we first check if `r == I 42` and if it is true we returns `()`, the Unit type,  and the transaction is done. In any other case, we raise an error with help of the `traceError` function, which shows the string we pass it as the error message on the console.


*Quick note: `I 42` is just an object of type _Data_, as `I` is one of the possible constructors of this data type (reference: [Data type](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/Data.hs))



## Validator Script

To translate this validator Haskell function to an actual Plutus validator, we need to compile it. Then it will actually be of type _Validator_. To this end we use the function `mkValidatorScript`, which takes as an argument something of type _CompiledCode_ of `(Data -> Data -> Data -> ())`. As an example:

```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

*Quick note: the line `{-# INLINABLE mkValidator #-}` (called _inlinable pragma_) that appears over the definition of the validator function tells the Plutus compiler to inline the validator function logic.  This allows the compiler to use it as a valid argument for the `PlutusTx.compile` function and to effectively compile it.


## Validator's Hash and Script's Address

After building our validator script, we must generate its _Hash_ and _Address_. The necessary code to do it is just boiler plate: it will always be almost the same for every contract we write in Plutus. The code explains itself.

+ Hash:
```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator
```

+ Address:
```haskell
scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash
```

## Typed Validator

It is possible to substitute the _Data_ type arguments of the validator by more suitable types of data. We can, for example, rewrite the second validator function we saw previously by:

```haskell
mkValidator :: () -> Integer -> ValidatorCtx -> Bool
mkValidator () r _
	| r == 42   = True
    	| otherwise = False
```
The Datum and Redeemer types _Unit_ and _Integer_, respectively, are the custom ones, while the Context _ValidatorCtx_ and the _Bool_ types are the ones will usually use in most cases. As this is the case, the Plutus team has taken care of it and the only adjustments we need to do are those related with the Datum and the Redeemer. This is because, as we have already seen, the compiler expects a validator of type `Data -> Data -> Data -> ()`, so we must add some code to do the trick. This code, which is also boiler plate, reads:

```haskell
data Typed
instance Scripts.ScriptType Typed where
	type instance DatumType Typed    = ()
    	type instance RedeemerType Typed = Integer
```

To compile this typed version of the validator we, again, have to make use of some boiler plate code. I don't understand this code very well, so I'll just copy-paste it:

```haskell
inst :: Scripts.ScriptInstance Typed
inst = Scripts.validator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer
```

One thing worth mentioning here is the last line of code. When defining `wrap` you use the function `wrapValidator` from module `Scripts` and you pass it the _custom_ data types you used in the validator function (preceded by `@`) as parameters. In this case, those data types were the `Unit` type for _Datum_ and the `Integer` type for the _Redeemer_. Then, we just need to change a bit the validator function to convert this compiled code to a _Validator_ type object:

```haskell
validator :: Validator
validator = Scripts.validatorScript inst
```

As we can see we have just passed `inst` to a new function called `validatorScript` from module `Script` without the need to compile it, as we already did it on the previous piece of code.

In principle, the data types we can successfully use when defining the validator are those defined as instances of the [isData class](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/IsData/Class.hs). This is due to the fact that is this class the one in charge to convert our custom data types in objects of _Data_ type. It does so be means of the methods `toData` and `fromData`. Anyway, if we want to use different data types from those instantiated in the referred link, we just need to define this instances. But this might be a very tedious process, so Plutus give us a convenient way to do it easily. For example, if we want to use some custom data type `fabulousRedeemerType`, we just have to add on top of our validator function these lines:

```haskell
newtype fabulousRedeemerType = fabulousRedeemerType Integer
	deriving Show

PlutusTx.unstableMakeIsData ''fabulousRedeemerType
```

The first two lines define my type while in the fourth one we pass this type to the helper function `unstableMakeIsData` as its argument, for which we use two single quotes, and end up having instantiated our new type.
