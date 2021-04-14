# Ubuntu setup for Plutus Playround

## Steps

* Clone plutus repo from [here](https://github.com/input-output-hk/plutus)
* Install NIX from [here](curl -L https://nixos.org/nix/install | sh)
* From terminal do the following:

```ssh
sudo mkdir -p /etc/nix/
mkdir -p ~/.config/nix
sudo touch /etc/nix/nix.conf
touch ~/.config/nix/nix.conf
```

* Paste the following in `~/.config/nix/nix.conf` and `/etc/nix/nix.conf`

```ssh
substituters = https://hydra.iohk.io/ https://iohk.cachix.org/ https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

* Navigate to the Plutus folder (from the Git Clone in Step 1) and paste this:

```ssh
nix build -f default.nix plutus.haskell.packages.plutus-core
```

* Open two terminal windows.
* In terminal window 1 (from the root of the Plutus project):

```ssh
nix-shell
cd plutus-playground-server
plutus-playground-server
```

* In terminal window 2 (from the root of the Plutus project):

```ssh
nix-shell
cd plutus-playground-client
npm run start
```
