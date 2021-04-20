# Plutus API documentation

## Local build

If you've built Plutus with Nix, documentation you shoud be ready as a symbolic link under `~/plutus` folder. Look for the symbolic link which contains _haddock_<sup>1</sup>:

```bash
# the documentation folder includes the word haddock    -|
result -> /nix/store/p9zqm03qmc7p5p1vfdbl06xysm85ir4f-haddock-join/
```

If this folder isn't under `~/plutus` you can build it with the command

```bash
[~/plutus] nix-build -A plutus-playground.haddock
```

You can see the plutus documentation in your regular browser. for example:

```bash 
brave-browser ~/plutus/result/share/doc/index.html
```


[^1]: haddock is haskell's tool for documentation generation


## How to use the documentation

The plutus documentation is a big static webpage and It includes many modules not directly related with Plutus Contracts, like Plutus Core, Plutus IR or the Playground backend. 

The most interesting modules for contract coding can be found at the very top of index.html page. Those are:

```
PlutusTx: Compiling Haskell to PLC (Plutus Core; on-chain code).
PlutusTx.Prelude: Haskell prelude replacement compatible with PLC.
Plutus.Contract: Writing Plutus apps (off-chain code).
Ledger.Constraints: Constructing and validating Plutus transactions. Built on PlutusTx and Plutus.Contract.
Ledger.Typed.Scripts: A type-safe interface for spending and producing script outputs. Built on PlutusTx.
Plutus.Trace.Emulator: Testing Plutus contracts in the emulator.
```