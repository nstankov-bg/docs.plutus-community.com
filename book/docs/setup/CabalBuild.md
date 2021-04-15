
# Using Cabal Build

### Do NOT waste time building Cabal locally. It's provided in Nix.

_**Note:**_
**The setup assumes that you are running this from an already configured Nix-Shell, look at the [other guides](http://docs.plutus-community.com/) to set this up.**

## Installation

Let's say that I have done a git clone on 2 repositories.

1. [Plutus](https://github.com/input-output-hk/plutus)
2. [Plutus-Pioneer-Program](https://github.com/input-output-hk/plutus-pioneer-program)

Both of them, I have decided to put in ```/opt/``` for the example.

```ssh
cd /opt/plutus
nix-shell
```

## From Nix-Shell
```ssh
cd /opt/plutus-pioneer-program/code/week1/
cabal update
cabal build
```
