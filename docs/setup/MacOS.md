# MacOS Setup for Plutus Pioneer Program

## Credits
Cloned from [Reddit](https://www.reddit.com/r/cardano/comments/mmzut6/macos_plutus_playground_build_instructions/)

Go give u/RikAlexander karma!

## Notes

Should work on Catalina and Big Sur. (Was tested on 2 Macs with Big Sur)  

## Setup

1 - Install Nix

    [$] sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
    

2 - Close terminal & reopen (to make sure that all environment variables are set)

3 - Check Nix installation / version with

    [$] nix --version
    

4 - Edit the /etc/nix/nix.conf file

    [$] nano /etc/nix/nix.conf
    

5 - Add these lines to the file:

    substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
    trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
    

_**Note:**_ These lines are there to avoid very long build times

_**Note 2:**_ if the file /etc/nix/nix.conf doesn't exist: create it. (`[$] mkdir /etc/nix` for the directory and `[$] touch /etc/nix/nix.conf` for the file)

6 - Restart your computer

7 - Now to install, clone the git repo first

    [$] git clone https://github.com/input-output-hk/plutus.git
    

8 - All the following builds should be executed while in the plutus directory

    [$] cd plutus
    

9 - Build the Plutus Core (This may take some time :) be patient)

    [$] nix build -f default.nix plutus.haskell.packages.plutus-core.components.library
    

_**Note:**_

On MacOS BigSur some users have reported that the building failed with an error like:

    error: while setting up the build environment: getting attributes of path '/usr/lib/libSystem.B.dylib': No such file or directory
    

To resolve this, we will change the nix build to an unstable (read: newer) build of nixpkgs.

    [$] sudo nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
    

10 - Build the Plutus Playground Client / Server

    [$] nix-build -A plutus-playground.client
    [$] nix-build -A plutus-playground.server
    

11 - Build other plutus dependencies

    [$] nix-build -A plutus-playground.generate-purescript
    [$] nix-build -A plutus-playground.start-backend
    [$] nix-build -A plutus-pab
    

12 - Go into nix-shell

    [$] nix-shell
    

13 - inside of the nix-shell

    [$] cd plutus-pab
    [$] plutus-pab-generate-purs
    [$] cd ../plutus-playground-server
    [$] plutus-playground-generate-purs
    

14 - start the playground server

    [$] plutus-playground-server
    

  

**Great! All set.**

  

15 - Now in a new terminal window:

    [$] cd plutus
    [$] nix-shell
    [$] cd plutus-playground-client
    

16 - Here we compile / build the frontend of the playground

    [$] npm run start
    

  

**We're done!**

The playground should be up and running.

Open your finest browser and navigate to:

[https://localhost:8009/](https://localhost:8009/)

Cloned from [Reddit](https://www.reddit.com/r/cardano/comments/mmzut6/macos_plutus_playground_build_instructions/)

Go give u/RikAlexander karma!