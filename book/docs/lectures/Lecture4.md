# Week04

### Credits

[Alberto Calzada - albertoCCz](https://github.com/albertoCCz)

# The Contract Monad
The purpose of the Contract monad is to define off-chain code that runs in the wallet. It has four parameters:
```haskell
-- Contract w s e a
```
where:
- a:  The overall result of the computation.
- w:  Allows you to write log messages of type `w`. The equivalent of this would be the list of strings we had on the _Writer.hs_ example.
- s:  Describes the blockchain capabilities aka what contract specific actions this contract can perform. For example: waiting for slots, submitting transactions, finding out your own public key or specific endpoints.
- e:  Describes the type of error messages.

### Simple example of Contract Monad
In the next example we will just send a log message from the contract to the console, so we will ignore the `a` and `w` types.
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
-- ^ Add extensions on top of the module file.

myContract :: Contract () BlockchainActions Text ()
myContract = Contract.logInfo @String "Hello from the contract!"
```
As you can see, we have chosen `s` (which I believe stands for "schema") to be of type `BlockchainActions`. This data type contains the minimal set of actions for a contract: from the options given above, we will not be able to use specific endpoints. In particular, you can check what actions are available in the [Contract.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract.hs) module if you look for `BlockchainActions`.

To use this contract you first need to define the trace, which substitutes what we previouly did on the Plutus Playground by providing with a list of actions the wallet(s) associated to the contract is going to perfom. The trace function we are going to use is the next one:
```haskell
myTrace :: EmulatorTrace ()
myTrace = void $ activateContractWallet (Wallet 1) myContract
```
This function just activates a wallet or set of wallets (Wallet 1 in this case) by associating it with a contract (myContract in this case) and normally the result is saved in a handler that we can use later. Normally, the code would look like this:
```haskell
myTrace :: EmulatorTrace ()
myTrace = h <- activateContractWallet (Wallet 1) myContract
```
where `h` is the handler. Now, as we are just interested in showing the log message and we will not use the handler, we use the `void` keyword so the compiler does not complaint. Finally, after defining the trace, we can define one or more test(s) to study whether or not the contract works as expected. This time we only define one of these tests because, again, we are just interested in the log message. The test is:
```haskell
myTest :: IO ()
myTest = runEmulatorTraceIO myTrace
```
Be aware of the `IO` at the end of the function `runEmulatorTraceIO`, as the function called `runEmulatorTrace` also exists. The difference between them is that the first one, the one we are using in our example, shows a compact and nicely formatted message on the console when executing (though also less informative) while the second one shows pages and pages of data that needs to be processed to make it readable.

With all this, we are ready to try our first contract on the repl. To do this, I have found that the simplest way is to:
1. Activate the `nix-shell` inside the plutus repo directory: `__@__:~/plutus$ nix-shell`
2. Move to `plutus-pioneer-program/code/week04/`
3. Access the repl: `cabal repl`
4. Load the _Contract.hs_ module `:l src/Week04/Contract.hs`
5. And executing the test: `myTest`

You will be shown something along the lines of:
```
Prelude Week04.Contract> test1
Slot 00000: TxnValidate af5e6d25b5ecb26185289a03d50786b7ac4425b21849143ed7e18bcd70dc4db8
Slot 00000: SlotAdd Slot 1
Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
  Contract instance started
Slot 00001: *** CONTRACT LOG: "Hello from the contract!"
Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
  Contract instance stopped (no errors)
Slot 00001: SlotAdd Slot 2
Final balances
Wallet 1:
    {, ""}: 100000000
Wallet 2:
    {, ""}: 100000000
Wallet 3:
    {, ""}: 100000000
Wallet 4:
    {, ""}: 100000000
Wallet 5:
    {, ""}: 100000000
Wallet 6:
    {, ""}: 100000000
Wallet 7:
    {, ""}: 100000000
Wallet 8:
    {, ""}: 100000000
Wallet 9:
    {, ""}: 100000000
Wallet 10:
    {, ""}: 100000000
```
You can observe, almost at the top of the console output, the contract log message we wrote. This is great!

## Throwing vs Handling errors
When executing a contract, as with any other piece of code, an error can happen. The behaviour of errors inside the contract monad is the expected one: the execution stops and an error message is shown in the console. To explore this a bit and to see the difference with the log message we just studied, let us add a line of code to the contract code which ends up as follows:
```haskell
myContract1 :: Contract () BlockchainActions Text ()
myContract1 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "Hello from the contract!"
```
If we run the test again (do not forget to change the contract name from `myContract` to `myContract1` in the trace, or to define a new one along with a new test), we will be prompted with something like this:
```
Prelude Week04.Contract> test1
Slot 00000: TxnValidate af5e6d25b5ecb26185289a03d50786b7ac4425b21849143ed7e18bcd70dc4db8
Slot 00000: SlotAdd Slot 1
Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
  Contract instance started
Slot 00001: *** CONTRACT STOPPED WITH ERROR: "\"BOOM!\""
Slot 00001: SlotAdd Slot 2
Final balances
Wallet 1:
    {, ""}: 100000000
Wallet 2:
    {, ""}: 100000000
Wallet 3:
    {, ""}: 100000000
Wallet 4:
    {, ""}: 100000000
Wallet 5:
    {, ""}: 100000000
Wallet 6:
    {, ""}: 100000000
Wallet 7:
    {, ""}: 100000000
Wallet 8:
    {, ""}: 100000000
Wallet 9:
    {, ""}: 100000000
Wallet 10:
    {, ""}: 100000000
```
Et voilà! You can see that now the execution process does not make it to the log message line, but it stops when we throw the error and it shows the error message we defined. Ok, this is great but wouldn't it be even greater if we could handle these errors? The fact is that we can.

To do so we need to define a new contract. This contract's only function is to handle the error(s) and it does so by means of the `handleError` function, which is defined at [Types.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract/Types.hs) inside the plutus-contract package. This function type is
```haskell
handleError :: forall w s e e' a.
  (e -> Contract w s e' a)  -- first argument type
  -> Contract w s e a       -- second argument type
  -> Contract w s e' a      -- return type
```
We see that its first argument is a function that takes an error type and returns a new contract where the writer (w), schema (s) and computation result (a) types are the same but where the error type (e) might change by the error handler. The second argument is the contract whose error we want to take care of. Let us see a contract example that handles the error we throw in `myContract1`. The code reads as follows:
```haskell
myContract2 :: Contract () BlockchainActions Void ()
myContract2 = Contract.handleError
  (\err -> Contract.logError $ "Caught error: " ++ unpack err)
  myContract1
```
We have chosen the `Void` error type of this second contract. As this data type has no inhabitant in Haskell, this means that this contract can not have any errors. We do this in order to show that the error from the first contract is indeed handled. As we can see, the function that handles the error just take this error and we unpack it to convert it to the `String` type (at this moment it is of type `Text`, as we declared it with the parameter `e` on `myContract1`). Then we append it to the string message and log it as an error to the console with `Contract.logError`. Finally, if we simulate this contract using the trace as we have already learned, we are shown a message like the next one:
```
Prelude Week04.Contract> test2
Slot 00000: TxnValidate af5e6d25b5ecb26185289a03d50786b7ac4425b21849143ed7e18bcd70dc4db8
Slot 00000: SlotAdd Slot 1
Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
  Contract instance started
Slot 00001: *** CONTRACT LOG: "Caught error: BOOM!"
Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
  Contract instance stopped (no errors)
Slot 00001: SlotAdd Slot 2
Final balances
Wallet 1:
    {, ""}: 100000000
Wallet 2:
    {, ""}: 100000000
Wallet 3:
    {, ""}: 100000000
Wallet 4:
    {, ""}: 100000000
Wallet 5:
    {, ""}: 100000000
Wallet 6:
    {, ""}: 100000000
Wallet 7:
    {, ""}: 100000000
Wallet 8:
    {, ""}: 100000000
Wallet 9:
    {, ""}: 100000000
Wallet 10:
    {, ""}: 100000000
```
where, as we see, the message is no longer an error but a log from the contract that informs us about the error. As the error data type of `myContract2` is of type `Void` we can be sure it was handled.

## The Schema parameter: s
We can define a custom set of contract actions by adding this actions to the `BlockchainActions` type. For example, let us say we want to add and endpoint called 'foo'. We just need to give a pseudonym to the set of action data types like this:
```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- ^ Add extensions on the top of the module file

type MySchema = BlockchainActions .\/ Endpoint "foo" Int
```
In the last line we define the type of the set of actions and we call it `MySchema`. Then we use the operator `.\/`, which acts on types, not on values, to "add" the endpoints that we want, in this case the `foo` endpoint. The first argument to `Endpoint` is a type level string which represents the name of the endpoint, and the second argument is the parameter type (which type of value this endpoint takes).

Once we have defined the endpoint, we can take the action defined by it using the trace emulator. First, we define our contract:
```haskell
myContract :: Contract () MySchema Text ()
myContract = do
    n <- endpoint @"foo"
    Contract.logInfo n
```
This contract just waits for some wallet to call the `"foo"` endpoint with some `Int` value and then logs it to the console. Then, we define the trace of the simulation, where now we can use the endpoint we have just defined:
```haskell
myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (Wallet 1) myContract
    callEndpoint @"foo" h 42
```
Note that now we are interested in calling the endpoint, for which it is necessary to use the handler. We do not use the `Void $ activateCon...` anymore, but we reference the function output with some variable, `h` (for handler) in this case. Finally we define the test function that runs this trace:
```haskell
test :: IO ()
test = runEmulatorTraceIO myTrace
```

## The Writer parameter: w
This type parameter can not be of any type but an instance of the type class `Monoid`. An example of data type which is an instance of this class is `List`. This parameter of the Contract monad is essential because it allows us to bring information back from the contract to the trace and also to the _PAB_, the Plutus Application Backend. We will be able to pass info back from the contract running in the wallet to the outside world. Let us see an example:
```haskell
myContract :: Contract [Int] BlockchainActions Text ()
myContract = do
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10
```
In the execution of this contract we first wait for 10 Slots, then we pass info back (which has to be of type `[Int]`, as we chose on the contract type declaration) using the `tell` statement, then we again wait for 10 Slots, and so on.

Now we define the trace as follows:
```haskell
myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (Wallet 1) myContract
    
    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs
    
    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys
    
    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs
```
With this trace, we are just observing the state communicated by the contract after a number of Slots. In particular, we wait 5 Slots and observe the state using the `observableState` function, to which we pass the handler `h` of the contract associated with the wallet. Because the first communication made by the contract happens after Slot 10, we will get an empty* list on the console. Then we wait for another 10 Slots and ask again for the state. Now the contract has already communicated something, as we have passed Slot 15 and the communications happened on Slot 10. In particular, we will observe the list `[1]` on the console. I'll let you guess what happens when we take a look at the contract state for the third time.

*_Quick note_: if you are asking how can it return an empty list, just remember that we imposed that the Writer parameter type had to be an instance of the type class `Monoid`. This type class implements three functions, the first one being `mempty :: a` which just gives you the empty object of your data type instance. In this case, our data type instance is a `List`, so the empty object is `[]`. (Sorry for the terminology, as I am still far from being fluent in Haskell).
