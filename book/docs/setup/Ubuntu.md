# Ubuntu setup for Plutus Playround

## Credits

[@dino-](https://github.com/dino-)

[@lsmor](https://github.com/lsmor)

[@gethashset](https://github.com/gethashset)

[@nstankov-bg](https://github.com/nstankov-bg)

[@dauta](https://github.com/dauta)


## Notes

These instructions were tested on Ubuntu 20.04

**Note** Part of what Nix will be doing is downloading a lot of build artifacts
from IOG to make the compilation processes take a lot less time. It means the
difference between 10 minute builds versus 3+ hour builds. The down-side is
this data will be quite large on your local system and you should expect
another 15G minimum to be used by it. Something to think about before going
further, if more disk space is needed than you have on your system.

## Install Nix and set up cache

Nix can be installed single-user or multi-user

More detailed info can be found in the
[Nix Package Manager Guide](https://nixos.org/manual/nix/stable)


### single-user

Single-user Nix installation has advantages.
 - No daemon and socket are created
 - A group of 32 nix users doesn't get created on the system
 - Nothing is written into `/etc`

The single-user Nix installer requires curl

```bash
sudo sh -c 'apt update && apt install curl'
```

Install nix

```bash
sh <(curl -L https://nixos.org/nix/install) --no-daemon
```

The installer should create the `/nix` directory for you with the proper
permissions. When it's done you will see

```bash
Installation finished!  To ensure that the necessary environment
variables are set, either log in again, or type

  . /home/<youruser>/.nix-profile/etc/profile.d/nix.sh
```

Execute the above command now to set the environment in this shell (also
logout/login will achieve this)

Make sure the changes the installer just made to your `~/.profile` make sense.

Next we will add IOG's caches to Nix to speed up our development significantly
by using their build artifacts. This is very important and means the difference
between 3+ hours and less than 10 minutes!

```bash
mkdir ~/.config/nix
echo 'substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/' >> ~/.config/nix/nix.conf
echo 'trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> ~/.config/nix/nix.conf
```


### multi-user

If you decide you'd like to go with multi-user Nix, read on.

The multi-user Nix installer requires curl and rsync

```bash
sudo sh -c 'apt update && apt install curl rsync'
```

Install nix

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```

This will run a wizard, prompting you for some things. When it's done we need
to set the environment in this shell (also logout/login will achieve this)

```bash
. /etc/profile.d/nix.sh
```

Next we will add IOG's caches to Nix to speed up our development significantly
by using their build artifacts. This is very important and means the difference
between 3+ hours and less than 10 minutes!

```bash
sudo sh -c "echo 'substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/' >> /etc/nix/nix.conf"
sudo sh -c "echo 'trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> /etc/nix/nix.conf"
sudo systemctl restart nix-daemon.service
```

Optionally add your shell account to the Nix trusted users in order to apply the
Nix trusted public keys for your builds, e.g.:

```bash
sudo sh -c 'echo "trusted-users = $0" >> /etc/nix/nix.conf' `whoami`
sudo systemctl restart nix-daemon.service
```

### After either Nix installation method

If everything worked you should be able to run a nix program

```bash
nix-env --version
```

and see output like this, version may vary

```bash
nix-env (Nix) 2.3.14
```

## Build the Plutus Playground server and client and start it

We now need to get the plutus-apps repo which contains the libraries for
working on Plutus and the Plutus Playground server.

```ssh
git clone https://github.com/input-output-hk/plutus-apps
cd plutus-apps
```

**Note** If you will be working on the Plutus Pioneer Project, it will be
necessary at this point to `git checkout ...` a specific commit to match the
class materials. That commit hash is listed in the exercises week## directory
in the `cabal.project` file. For more info on this, see [Working on contracts with and without cabal build](https://docs.plutus-community.com/docs/setup/CabalBuild.html)

Now we will run the Plutus Playground servers. We start these in a `nix-shell`
which sets up the environment and has working versions of tools.

**Note** Optionally, a project exists to control the two Plutus Playground
servers as a systemd service, making start/stop/restart much less manual. See
[plutus-playground-systemd](https://github.com/dino-/plutus-playground-systemd)
for installation and configuration. If you use this, skip past the "terminal
window" instructions below and access your new playground server in a browser.

Open two terminal windows

In terminal window 1

```ssh
cd plutus-apps
nix-shell
cd plutus-playground-server
plutus-playground-server
```

If it's successful, you should see `Interpreter ready`

In terminal window 2

```ssh
cd plutus-apps
nix-shell
cd plutus-playground-client
npm run start
```

If it's successful, you should see `[wdm]: Compiled successfully.`

You should now be able to navigate to <https://localhost:8009>. The browser
will complain about it being a risky website, allow it.


## Miscellaneous

### Completely uninstalling Nix

Nix (unfortunately) installs with a non-distro-specific and not-reversible
method and so requires careful unistallation if you don't want it any longer.
The Nix documentation says simply removing `/nix` is the way but this leaves a
lot of unneeded files on your system. We can clean this up properly.

These instructions work equally well for single- or multi-user Nix.

First, disable and stop the systemd units if they exist. This is harmless to
do if they don't exist.

```bash
sudo systemctl disable --now nix-daemon.service
sudo systemctl disable --now nix-daemon.socket
```

Then delete the many directories and files that are on the system

```bash
rm -rf $HOME/{.nix-*,.cache/nix,.config/nix}
sudo rm -rf /root/{.nix-channels,.nix-defexpr,.nix-profile,.config/nixpkgs,.cache/nix}
sudo rm -rf /etc/{nix,profile.d/nix.sh*}
sudo rm -rf /nix
```

Now we will remove the 32(!) `nixbld` users that were added to the system for a
multi-user installation.

```bash
sudo sh -c 'for N in $(seq 32); do deluser "nixbld$N"; done'
```

Finally, if you see one, remove the line that was added to your
`$HOME/.profile` that sources the nix environment.
