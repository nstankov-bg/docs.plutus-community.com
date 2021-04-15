# Prerequisites

In order to setup your favorite editor for Haskell/Plutus development you need following components
- GHC (haskell compiler) & cabal (build system for haskell projects)
- [haskell-language-server](https://github.com/haskell/haskell-language-server/)

## GHC & cabal
You have 2 options for setting up the compiler infrastructure:
- _Using Nix and nix-shell_. Assuming you've done setup of [plutus](https://github.com/input-output-hk/plutus) for running `plutus-playground` locally, you already have `ghc` and `cabal` binaries available inside `nix-shell`. 
- _System-wide installation_. Generally, if you don't want to run `plutus-playground` locally and only want to compile Plutus contracts, you can use your existing haskell installation or install the compiler toolchain without Nix. For using this method, follow the instructions on [ghcup website](https://www.haskell.org/ghcup/).

## HSL (haskell-language-server)
IDE-like features like auto-completion, type hints, docs and jump-to-definition are provided by a tool called [haskell-language-server](https://github.com/haskell/haskell-language-server/). It is a standalone program that implements [Language Server Protocol (LSP)](https://langserver.org/) for Haskell. Your editor can talk to this program and get the information for showing types, doing auto-completion, etc.

### Linux
On Linux, installing the HSL is pretty straightforwad. If you've done Nix setup then you already have `haskell-language-server` binary on your `$PATH` inside `nix-shell`. 
If you've done installation with `ghcup`, you can install HSL with following command
   ```bash
   ghcup install hsl
   ```
or you can download pre-built binary from [releases page](https://github.com/haskell/haskell-language-server/releases). Don't forget to put the binary on your `$PATH`. 

### macOS
On macOS, the installation is a bit trickier. 
Due to [linking proglems](https://github.com/haskell/haskell-language-server/issues/1160) HSL crashes when your project uses TemplateHaskell (which Plutus uses a lot) ([issue](https://github.com/haskell/haskell-language-server/issues/1431), [issue](https://github.com/haskell/haskell-language-server/issues/277)). The workaround for this is to compile HSL manually with dynamic linking support. 

1. Make sure you have ghc & cabal installed and available on your `$PATH` (either locally or within `nix-shell`)
2. Clone HSL repo
   ```bash
   $ git clone https://github.com/haskell/haskell-language-server --recurse-submodules
   $ cd haskell-language-server
   ```
3. Make changes to `haskell-language-server.cabal` file to use dynamic linking (huge shoutout to `f` for finding that). The [diff](https://github.com/haskell/haskell-language-server/issues/1160#issuecomment-756566273)
4. Find out your ghc version
   ```bash
   ghc --version
   # The Glorious Glasgow Haskell Compilation System, version 8.10.4
   ```
5. Build HSL for your compiler version
   ```bash
   ./cabal-hls-install hls-<version>
   # cabal-hls-install hls-8.10.4
   ```
6. Once the build is done, newly created binary will be located in `$HOME/.cabal/bin`. If you use `nix-shell`, make sure that you use right binary since nix-shell provides its own. 


Once you have everything set up you can proceed to configuring your editor.