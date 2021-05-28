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


> haddock is haskell's tool for documentation generation


## (Optional) launch Haddock documentation from a local static site web server

The method above will open the Plutus API documentation using `file://` procotol, which can lead to problems dealing with javascript, mainly haddock's search utility is disable. We can use plutus dependencies available within the `nix-shell` to effortless create a local static site web server to serve the documentation using `http` protocol.

- Open a nix shell in plutus folder

```bash
> cd path/to/plutus
> nix-shell
```

- Move out of the plutus folder an create a new folder to store the web server executable

```bash
[nix-shell:path/to/plutus] > mkdir path/to/haddock-web
[nix-shell:path/to/plutus] > cd path/to/haddock-web
[nix-shell:path/to/haddock-web] > echo "module Main where" > main.hs
```

- Open main.hs in your editor and copy-paste the following code. Notice that no dependencies have to be installed because we are re-using plutus' ones

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Servant.Server.StaticFiles as Static
import qualified Servant.Server as Server
import Servant.API ( Raw, type (:>) )
import Servant
    ( Proxy(..),
      Application,
      Raw,
      type (:>),
      serve,
      serveDirectoryWebApp)
import Data.Kind ()
import Network.Wai.Handler.Warp (run)
import System.Directory ( getSymbolicLinkTarget )
import System.FilePath.Posix ((</>))
import System.Environment (getArgs)
import System.Exit as Exit ( die )

type Haddock = "plutus-haddock" :> Raw

serverPort :: Int
serverPort = 8081

server :: FilePath -> Server.Server Haddock
server = serveDirectoryWebApp

myApi :: Proxy Haddock
myApi = Proxy

app :: FilePath -> Application
app = serve myApi . server

main :: IO ()
main = do
    args <- getArgs
    rootPath <- 
      case args of
        "-s":pathToHaddockSymLink:_  -> getSymbolicLinkTarget pathToHaddockSymLink
        "-p":pathToNixStoreHaddock:_ -> return pathToNixStoreHaddock
        _                            -> die "unrecognize input params. Use -s <path/to/haddock/symbolic/link> or -p <path/to/nix/store/haddock/> "
    let indexPath = rootPath </> "share" </> "doc"
    putStrLn $ "running plutus documentation in http://localhost:" <> show serverPort <> "/plutus-haddock/index.html"
    run serverPort $ app indexPath
```

- Compile `main.hs` file directly with `ghc` into an executable.

```bash
#         This is the executable's name --|
[nix-shell:path/to/haddock-web] > ghc -o plutus-haddock main.hs
[1 of 1] Compiling Main             ( main.hs, main.o )
Linking plutus-haddock ...
```

- you can run the executable with either flags `-s` or `-p` and the path to haddock (symbolic or actual nix-store path resp.). For example, using week's 5 plutus commit you can run any of the following:
        
   - `plutus-haddock -s ~/plutus/result`
   - `plutus-haddock -p /nix/store/7h65546y7f37h7ma1p16n2dcnmbsx5d1-haddock-join/`

  please notice that `/nix/store` path depends on `plutus`'s commit, which changes every week. Whereas the symbolic link hasn't changed since week01 but you have to build it manually using the instructions at the begining of this document.

- When runnig the executable you should see the follwing output

```haskell 
[path/to/haddock-web] > ./plutus-haddock -s <path>/<to>/<plutus>/result # notice that nix-shell isn't necessary anymore
running plutus documentation in http://localhost:8081/plutus-haddock/index.html
```

- now your plutus documentation is running in the given URL with js enable. 

## How to use the documentation

The plutus documentation is a big static webpage and It includes many modules not directly related with Plutus Contracts, like Plutus Core, Plutus IR or the Playground backend. If you launch haddock from a web server, you can press `s` to open the _search prompt_ which is super handy to discover unknow functions and to find the module a function is exported from.

The most interesting modules for contract coding can be found at the very top of index.html page. Those are:

```yaml
PlutusTx: Compiling Haskell to PLC (Plutus Core; on-chain code).
PlutusTx.Prelude: Haskell prelude replacement compatible with PLC.
Plutus.Contract: Writing Plutus apps (off-chain code).
Ledger.Constraints: Constructing and validating Plutus transactions. Built on PlutusTx and Plutus.Contract.
Ledger.Typed.Scripts: A type-safe interface for spending and producing script outputs. Built on PlutusTx.
Plutus.Trace.Emulator: Testing Plutus contracts in the emulator.
```