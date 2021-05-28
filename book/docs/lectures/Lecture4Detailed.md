## Credits

Condensed version of Lecture #4 of the Plutus Pioneer Program by Lars Brünjes on [Youtube](https://www.youtube.com/watch?v=6Reuh0xZDjY)

Cloned from [Reddit (u/RikAlexander)](https://www.reddit.com/r/cardano/comments/n9c9wz/week_04_plutus_pioneer_program/)

## Note

Lecture 4 was _huge_ (2 hours+); mostly due to the explanation about `Monads`, I'll try and give a brief condensed version of it, but it's still recommended that you watch the lecture. Lars did an amazing job of explaining the concepts.

## Side Effects

The Lecture starts with Lars explaining `Side Effects` and it's problems.

Side effects in imperative programming languages are very common, due to the fact that functions / classes are able to _use or change_ values from outside of it's scope.

e.g. (pseudo code)

    public class mySideEffect() {

        private int number;

        public function setNumber(n) {
            this.number = n;
        }

        public function getNumber() {
            return this.number;
        }

    }

The problem lies in the fact that `getNumber` will not always give a value we expect.

The value may be changed with `setNumber`, this changes the value of `this.number`; thus `getNumber` will return something different every time we call it.

Functions in Haskell / Plutus are without side effects. They can't use values from outside of it's scope.

Meaning when we input the same values in a function, we will _always_ get the same results back.

```haskell
squared :: Int -> Int
squared a = a * a
```

Most basic function to demonstrate; if we give it the number 3 for example, it will ALWAYS return 9. The function is not able to use values outside of it's scope.

_Important_: this assures the `user` that every time we give a function some arbitrary value, it will always return what we expect it to return.

This gives us 2 (awesome) things:

1. refactoring/performance upgrades made easy. If we want to change the contents of a function to for example a faster version of its predecessor, we can do so, and be certain that everywhere this function was used, will still work.
2. less bugs. Using a functional programming language, enforces the developer to write code without side effects; thus reducing bugs / problems that are not easily traceable.

## IO

Now, I've been ranting about all code being side effects _free_, although this isn't completely true.

In order for our program to not be useless; we _need_ side effects..

We need input from the user, and output something to the user.

Here Lars referred to the video [Haskell is useless](https://www.youtube.com/watch?v=iSmkqocn0oQ).

So. How do we deal with this. Well for our much wanted side effects Haskell has the solution: IO. (IO = Input/Output)

```haskell
number :: IO Int
```

Instead of just `number :: Int`, we use the IO Type Constructor.

"What does IO mean? It is an action / recipe, to compute an Integer; this computation CAN invoke side effects"

Note: Referential transparency is not broken, IO Int is just a recipe. When evaluated, the recipe is not executed, it's only returning the recipe of getting an Int.

The only way to execute an IO action, is from `Main` of the program (or ghci which also supports/allows IO actions being executed). Same as for example Java, which also has it's `Main` function.

If we look at the IO implementation `:i IO`

```haskell
type IO :: * -> *
newtype IO a
= GHC.Types.IO (GHC.Prim.State# GHC.Prim.RealWorld
				-> (# GHC.Prim.State# GHC.Prim.RealWorld, a #))
	-- Defined in ‘GHC.Types’
instance Applicative IO -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance Monad IO -- Defined in ‘GHC.Base’
instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
instance Semigroup a => Semigroup (IO a) -- Defined in ‘GHC.Base’
instance MonadFail IO -- Defined in ‘Control.Monad.Fail’
```

IO is a newtype, and has multiple instances. (Applicative / Functor / Monad / Monoid / Semigroup and MonadFail). Monad is important for our smart contracts, we'll see why later on.

Check out all the implementations (with `:i`), `Functor`'s implementation for example (`:i Functor`)

```haskell
type Functor :: (* -> *) -> Constraint
class Functor f where
fmap :: (a -> b) -> f a -> f b
(<$) :: a -> f b -> f a
{-# MINIMAL fmap #-}
	-- Defined in ‘GHC.Base’
instance Functor (Either a) -- Defined in ‘Data.Either’
instance Functor [] -- Defined in ‘GHC.Base’
instance Functor Maybe -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance Functor ((->) r) -- Defined in ‘GHC.Base’
instance Functor ((,,,) a b c) -- Defined in ‘GHC.Base’
instance Functor ((,,) a b) -- Defined in ‘GHC.Base’
instance Functor ((,) a) -- Defined in ‘GHC.Base’
```

As you can see `Functor` can also have multiple instances, the ones we will be having a closer look at are `Either` and `Maybe`.

## Hello world!

```haskell
main :: IO ()
main = putStrLn "Hello World!"
```

The main function must be of type IO Unit.

It allows the main function to do some side effects, and in the end return Unit (void / nothing)

If we look at putStrLn and it's definition:

```haskell
putStrLn :: String -> IO ()
```

It receives a String and returns IO (), so this function is suitable for the `main` program.

(try it in ghci!)

## Monad

We'll start with multiple concepts with `Maybe` and `Either` and slowly work our way to "Monads"; by going through it this way, it's much easier to grasp the famous `Monad` concept later on.

## Maybe

`Maybe` returns either `Nothing` or a `Just`.

```haskell
data Maybe a = Nothing | Just a
```

Lars starts of with a function `foo` that receives 3 strings, and Maybe returns an Int (see what I did there? :D )

Lines 6 - 13 of Maybe.hs

```haskell
foo :: String -> String -> String -> Maybe Int
foo x y z = case readMaybe x of
	Nothing -> Nothing
	Just k  -> case readMaybe y of
		Nothing -> Nothing
		Just l  -> case readMaybe z of
			Nothing -> Nothing
			Just m  -> Just (k + l + m)
```

If we were to call this `foo` function with `foo "1" "2" "3"` it would return a Just Int of 6.

If we'd call it with `foo "1" "abc" "3"` it would return `Nothing`.

The function does 3 cases, each case tries to compute the input variable with the `readMaybe` haskell function (readMaybe checks if a string is an Integer, if so returns Just int, otherwise Nothing).

When the case gets `Nothing` we will break out of our cases, and just return nothing to our foo function; otherwise move on to the next parameter.

If we get to the last one, which is also evaluated to an Int, we'll return a `Just` of all values combined.

Now to improve this function, we could upgrade our code by using a function; this way we can eliminate all the duplicate case code.

Lines 15 - 23 of Maybe.hs

```haskell
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing  _ = Nothing
bindMaybe (Just x) f = f x

foo' :: String -> String -> String -> Maybe Int
foo' x y z = readMaybe x `bindMaybe` \k ->
			readMaybe y `bindMaybe` \l ->
			readMaybe z `bindMaybe` \m ->
			Just (k + l + m)
```

Here we've created the `bindMaybe` function. It's purpose is to receive a `Maybe` (which can be of either `Just` or `Nothing`), and an inline function.

```haskell
(a -> Maybe b)
```

Now each line

```haskell
readMaybe x `bindMaybe` \k ->
```

Says: execute `bindMaybe`, with the output of `readMaybe x`, and the anonymous function \\k. Now when the output of readMaybe is `Nothing`, just return Nothing and do NOT execute the anonymous function.

If it is Just Int? Great. Continue with the next variable.

This goes on until we've reached the last variable, once we've gotten here, we know: every variable MUST be of type Just Int (otherwise we wouldn't have gotten all the way down here). so: return a Just of all values combined.

## Either

`Either` is quite similar to `Maybe`, it will one of two values.

`Left` or `Right`.

What's nice about this, as opposed to `Maybe`, is that `Nothing` cannot hold a value other than it being.. Nothing.

`Left` or `Right` _can_ both hold values.

Lines 6 - 18 of Either.hs

```haskell
readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
	Nothing -> Left $ "can't parse: " ++ s
	Just a  -> Right a

foo :: String -> String -> String -> Either String Int
foo x y z = case readEither x of
	Left err -> Left err
	Right k  -> case readEither y of
		Left err -> Left err
		Right l  -> case readEither z of
			Left err -> Left err
			Right m  -> Right (k + l + m)
```

Its very similar to the first implementation of our Maybe function, the difference is that when readMaybe returns `Nothing`, we'll return a `Left` with an error message (can't parse ... ).

Now of course here we have the same redundant cases, which we can clean up by creating another helper function.

Lines 20 - 28 of Either.hs

```haskell
bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x)  f = f x

foo' :: String -> String -> String -> Either String Int
foo' x y z = readEither x `bindEither` \k ->
			readEither y `bindEither` \l ->
			readEither z `bindEither` \m ->
			Right (k + l + m)
```

It works the same as the second implementation of Maybe.hs (although we do need the readEither function for handling our error message `Left`)

## Writer

Next up, `Writer`.

First implementation:

Line 6 - 21 in Writer.hs

```haskell
data Writer a = Writer a [String]
	deriving Show

number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n]

tell :: [String] -> Writer ()
tell = Writer ()

foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs) =
let
	s = k + l + m
	Writer _ us = tell ["sum: " ++ show s]
in
	Writer s $ xs ++ ys ++ zs ++ us
```

We first create our custom data type `Writer`, which takes an Variable and a List of Strings. `deriving Show` makes sure we can easily output it's contents.

Then we make a function `number` which takes an Integer, and returns a Writer with that Integer and a List with one item in it, saying "number: ${number}".

The function `tell` takes a List with String(s) and creates a `Writer` with a type Unit for our Integer (void) and the List of String(s) as the second parameter.

Note: we could have written this as

```haskell
tell :: [String] -> Writer ()
tell xs = Writer () xs
```

But by not specifying it, haskell will automatically append it to the parameter list. (just syntactic sugar)

Now the foo function.

It takes 3 instances of `Writer`, and combines all the values to a final `Writer` instance.

For this we use `let`.

`let` in javascript is just defining a variable; which is more or less what we are doing here.

We first want to define `s = k + l + m`, which just sums up all the Integers to form a total.

Then we're calling `tell`, to give us a String List with the sum contents "sum: ${sum}", binding its String List output to `us`.

Now the `in` keyword, which is our final "return", gives a new `Writer` instance with the sum, and finally all of our String Lists combined as one list.

As such, when running

```haskell
foo (number 1) (number 2) (number 3)
```

Our output would be:

```haskell
Writer 6 [ "number: 1", "number: 2", "number: 3", "sum: 6" ]
```

## Second implementation

Lines 23 - 36 of Writer.hs

```haskell
bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a xs) f =
let
	Writer b ys = f a
in
	Writer b $ xs ++ ys

foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = x `bindWriter` \k ->
			y `bindWriter` \l ->
			z `bindWriter` \m ->
			let s = k + l + m
			in tell ["sum: " ++ show s] `bindWriter` \_ ->
				Writer s []
```

The second implementation uses the same logic as the `bindMaybe` and `bindEither` functions we created previously. I don't think it requires much more explanation, as i already did this for both the `bindMaybe` and `bindEither` functions.

## Monad

Now you might ask, what am I supposed to do with all these examples and scripts.

Well, it was all building up to this point: the Monad.

The definition of the Monad is this:

```haskell
Monad m where
	(>>=) :: m a -> (a -> m b) -> m b
	(>>) :: m a -> m b -> m b
	return :: a -> m a
```

Now I'll try to keep this as simple as possible.

The `bind` operator. (`>>=`)

```haskell
(>>=) :: m a -> (a -> m b) -> m b
```

This _should_ look familiar to you by now.

Because it does the same as all our `bindMaybe`, `bindEither` and `bindWriter` functions.

For reference:

```haskell
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindWriter :: Writer a -> (a -> Writer b) -> Writer b
```

The `then` operator

```haskell
(>>) :: m a -> m b -> m b
```

Basically does the same as the bind operator, only it does not care about the output. (it "throws" it away).

You could also just only use the bind operator and just not care about the output `a`.

Last but not least, the `return`

```haskell
return :: a -> m a
```

Don't think this needs much explanation. It just takes `a` and turns it into `a` of type `m`.

Now how to use this for e.g. `Writer`.

First of all we'll implement our Writer Monad:

```haskell
instance Functor Writer where
	fmap = liftM

instance Applicative Writer where
	pure = return
	(<*>) = ap

instance Monad Writer where
	return a = Writer a []
	(>>=) = bindWriter
```

Creating an instance of Monad where `m` is our `Writer`, and implement the `return` and `bind` (`>>=`) functions.

Now check the Monad.hs

Here we create a helper function for our "Int checking"

Lines 13 - 18 of Monad.hs

```haskell
threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz =
	mx >>= \k ->
	my >>= \l ->
	mz >>= \m ->
	let s = k + l + m in return s
```

Or by using the "do" syntax, it gets even easier (Lines 38 - 42 of Monad.hs):

```haskell
threeInts' :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts' mx my mz = do
	k <- mx
	l <- my
	m <- mz
	let s = k + l + m
	return s
```

which of course just means the same as before.

Now to use this (Lines 38 - 42 of Writer.hs):

```haskell
foo'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo'' x y z = do
	s <- threeInts x y z
	tell ["sum: " ++ show s]
	return s
```

Done.

Same for Maybe (Lines 25 - 26 of Maybe.hs)

```haskell
foo'' :: String -> String -> String -> Maybe Int
foo'' x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)
```

and Either (Lines 30 - 31 of Either.hs)

```haskell
foo'' :: String -> String -> String -> Maybe Int
foo'' x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)
```

This has multiple advantages:

1. Allows naming of common patterns. As Lars said "it's always useful to name things", and in this case our integer checking may be given a name. `threeInts`
2. We don't need all of our `bindMaybe`, `bindEither` and `bindWriter` names. We can always just use our `return` and `bind` (`>>=`)
3. We can easily create functions to be "shared", they don't care about which Monad instance, it only cares about the Monad type. Which means same as for our 3 bind functions, we can use the new `threeInts` function, for all of them. We don't care if its a Maybe / Either or Writer type.

Now for a more in depth explanation, I'd recommend watching the Lecture itself. The first 1.5 Hours are all about explaining the Monad.

Note: if we go back to IO

Since IO is of type Monad, we can use the `bind` / `then/sequence` operators, to "chain" multiple IO actions!

E.g. printing to screen/terminal

```haskell
putStrLn "Hello" >> putStrLn "World"
```

We don't care about the first `putStrLn` result, so to chain them we can use the `then/sequence` operator

Now for the `bind` operator, we could get input from the User, and output it back to the screen with `putStrLn`

```haskell
getLine >>= putStrLn
```

## EmulatorTrace Monad

Multiple pioneers have asked for a better way of testing contracts, without having to always copy code into the Playground (let alone running the playground server/client).

For this we have the EmulatorTrace Monad.

Check the [EmulatorTrace.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Trace/Emulator.hs) in the Plutus Repository

According to Lars the most important function here:

Lines 226 - 229

```haskell
runEmulatorTrace
	:: EmulatorConfig
	-> EmulatorTrace ()
	-> ([EmulatorEvent], Maybe EmulatorErr, EmulatorState)
```

This executes a trace on an emulated blockchain It expects an [EmulatorConfig](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Wallet/Emulator/Stream.hs), through which we can define an initial state of the blockchain.

E.g. `Value` in each wallet at the start of trace execution.

_Note:_ `Value` may also be native Tokens instead of only ada.

There is also a defaultDist (Lines 248 - 249 in [Trace.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract/Trace.hs)), which will provide 10 wallets with each 100 ada.

Also there is defaultDistFor (Lines 251 - 252 in [Trace.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract/Trace.hs)) where we can supply the function with a List of Wallets to fill with the default 100 ada. `defaultDistFor [Wallet 1]`

For getting a clean trace (one that we can easily inspect on the terminal)

    runEmulatorTraceIO $ return ()

For more configuration optionality there is also a `runEmulatorTraceIO'` which takes a `EmulatorConfig` for a different distribution if we want and it also takes a `traceConfig` all of which are defined in the [Emulator.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Trace/Emulator.hs)

`traceConfig` is defined on lines 198 - 203 of Emulator.hs, which has 2 functions, the `showEvent` and the `outputHandle` function.

`showEvent` by default uses `defaultShowEvent` (Lines 213 - 222 of Emulator.hs)

```haskell
defaultShowEvent :: EmulatorEvent' -> Maybe String
defaultShowEvent = \case
	UserThreadEvent (UserLog msg)                                        -> Just $ "*** USER LOG: " <> msg
	InstanceEvent (ContractInstanceLog (ContractLog (A.String msg)) _ _) -> Just $ "*** CONTRACT LOG: " <> show msg
	InstanceEvent (ContractInstanceLog (StoppedWithError err)       _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> show err
	InstanceEvent (ContractInstanceLog NoRequestsHandled            _ _) -> Nothing
	InstanceEvent (ContractInstanceLog (HandledRequest _)           _ _) -> Nothing
	InstanceEvent (ContractInstanceLog (CurrentRequests _)          _ _) -> Nothing
	SchedulerEvent _                                                     -> Nothing
	ChainIndexEvent _ _                                                  -> Nothing
	WalletEvent _ _                                                      -> Nothing
	ev                                                                   -> Just . renderString . layoutPretty defaultLayoutOptions . pretty $ ev
```

Where most events just return Nothing by default, these of course could be implemented if we need more information during the Trace execution.

The `outputHandle` by default uses `System.IO.stdout` but this may be changed to a file for example.

## Time to log!

Trace.hs of in the Week04 directory, first imports our week3 Vesting contract (copied into Week04)

    import Week04.Vesting

Creating our trace which we want to execute and inspect.

Lines 20 - 32 of Trace.hs

```haskell
myTrace :: EmulatorTrace ()
myTrace = do
	h1 <- activateContractWallet (Wallet 1) endpoints
	h2 <- activateContractWallet (Wallet 2) endpoints
	callEndpoint @"give" h1 $ GiveParams
		{ gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2
		, gpDeadline    = Slot 20
		, gpAmount      = 1000
		}
	void $ waitUntilSlot 20
	callEndpoint @"grab" h2 ()
	s <- waitNSlots 1
	Extras.logInfo $ "reached slot " ++ show s
```

This basically does what we used to do in the playground! (Awesome.)

If you followed along the last 4 weeks, this code should be self explanatory.

To run our `myTrace` (Lines 17 - 18 of Trace.hs)

```haskell
test :: IO ()
test = runEmulatorTraceIO myTrace
```

So now we can write multiple tests / traces and execute them to test our contract!

## Contract Monad

The purpose of the Contract Monad is to define off-chain code that runs in the Wallet.

Contract is defined with 4 parameters `Contract w s e a`

w -> Logging as in our `Writer` monad

s -> Blockchain specific capabilities (waiting slots, waiting for transaction, finding out own private key, handling specific endpoints)

e -> Blockchain specific capabilities - types of error messages

a -> The overall result of computation

First example (Lines 19 - 28 of Contract.hs):

```haskell
myContract1 :: Contract () BlockchainActions Text ()
myContract1 = do
	void $ Contract.throwError "BOOM!"
	Contract.logInfo @String "Hello from the contract!"

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (Wallet 1) myContract1

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1
```

_Note:_ the @String is there because the language extension `OverloadedStrings` is activated, by which quotes ("") are not only for String but also for other types (e.g. Text).

Now the compiler does not know which of Text/String we mean here.

By also activating the language extension `TypeApplications`, we can now specify the Type we want by writing @String or @Text in front of the quoted string.

_Note:_ @String works, and is in the Week04 Repo, although I think @Text would've been the logical choice here. Both work though.

Now test1 can be run from the terminal, which will then return a nice trace of our contract, displays our error "BOOM!" and of course disregards our "Hello from the contract" log message. (without the throwError this would be logged)

Second example (Lines 30 - 39 of Contract.hs)

```haskell
myContract2 :: Contract () BlockchainActions Void ()
myContract2 = Contract.handleError
	(\err -> Contract.logError $ "Caught error: " ++ unpack err)
	myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (Wallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2
```

Handles an error. (Run it, and look at the Trace output.)

Third example (41 - 54 of Contract.hs)

```haskell
type MySchema = BlockchainActions .\/ Endpoint "foo" Int

myContract3 :: Contract () MySchema Text ()
myContract3 = do
	n <- endpoint @"foo"
	Contract.logInfo n

myTrace3 :: EmulatorTrace ()
myTrace3 = do
	h <- activateContractWallet (Wallet 1) myContract3
	callEndpoint @"foo" h 42

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3
```

Which creates an endpoint `foo` that takes an Integer and logs this to the console.

`activateContractWallet` is now assigned to the `h` variable, because now we want to call the endpoint `callEndpoint` with an Integer of 42.

Fourth and final example (56 - 81 of Contract.hs)

```haskell
myContract4 :: Contract [Int] BlockchainActions Text ()
myContract4 = do
	void $ Contract.waitNSlots 10
	tell [1]
	void $ Contract.waitNSlots 10
	tell [2]
	void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
	h <- activateContractWallet (Wallet 1) myContract4

	void $ Emulator.waitNSlots 5
	xs <- observableState h
	Extras.logInfo $ show xs

	void $ Emulator.waitNSlots 10
	ys <- observableState h
	Extras.logInfo $ show ys

	void $ Emulator.waitNSlots 10
	zs <- observableState h
	Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
```

Example 4 looks at the `w` parameter of the Contract Monad.

First of all: it must be a `Monoid`

If we look at the definition of `Monoid` (`:i Monoid`)

```haskell
Monoid a where
	mempty :: a
	mappend :: a -> a -> a
	mconcat :: [a] -> a
```

`mempty` -> returns an empty value

`mappend` -> appends values together (although according to [haskell.org](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Monoid.html) "This method is redundant and has the default implementation mappend = (<>) since base-4.11.0.0. Should it be implemented manually, since mappend is a synonym for (<>), it is expected that the two functions are defined the same way. In a future GHC release mappend will be removed from Monoid.")

`mconcat` -> concatenates list values (`mconcat ["Hello", " ", "World", "!"] -> "Hello Haskell!"`)

We now have established bi-directional communication:

- using endpoints -> into Contract
- via tell mechanism -> out of Contract

## Homework

The Homework for week 4 is to build a trace for a simple Contract provided by Lars.

Lines 21 - 33 of Homework.hs

```haskell
data PayParams = PayParams
	{ ppRecipient :: PubKeyHash
	, ppLovelace  :: Integer
	} deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = BlockchainActions .\/ Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
	pp <- endpoint @"pay"
	let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
	void $ submitTx tx
	payContract
```

`payContract` recursively calls its self, so we can call the endpoint `pay` as often as we like.

It does nothing but "pay" an amount of lovelaces (`ppLovelace` of `PayParams`) to a wallet (`ppRecipient` of `PayParams`) using `mustPayToPubKey` from Ledger.Constrains.

## Our assignment

Lines 38 - 45 of Homework.hs

```haskell
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = undefined -- IMPLEMENT ME!

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
```

`payTest1` and `payTest2` are the Two tests ready to be used, our assignment is to implement the `payTrace` EmulatorTrace function as such:

- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as recipient, but with amounts given by the two arguments.
- There should be a delay of one slot after each endpoint call. (hint: `Emulator.waitNSlots`)

Everything covered in this article, _should_ suffice in finishing this homework assignment.

As always: it's recommended that you try to implement this yourself.

But for the sake of completeness:

## Homework Solution

```haskell
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
	h <- activateContractWallet (Wallet 1) payContract
	callEndpoint @"pay" h $ PayParams
		{ ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
		, ppLovelace  = x
		}
	void $ Emulator.waitNSlots 1
	callEndpoint @"pay" h $ PayParams
		{ ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
		, ppLovelace  = y
		}
	void $ Emulator.waitNSlots 1
```

## Footnote

Week04 Done!

Happy Coding! :)

## Credits

Condensed version of Lecture #4 of the Plutus Pioneer Program by Lars Brünjes on [Youtube](https://www.youtube.com/watch?v=6Reuh0xZDjY)

Cloned from [Reddit (u/RikAlexander)](https://www.reddit.com/r/cardano/comments/n9c9wz/week_04_plutus_pioneer_program/)
