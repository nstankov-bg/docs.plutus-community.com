# Plutus VM (Ubuntu Based)

## Credits
Original from: [edwint88](https://github.com/edwint88/plutus-vm)

Go give him a star!

## Prerequisites

1. Obtain & Install Vagrant from the [Official Source](https://www.vagrantup.com)
2. Obtain & Install VirtualBox from the [Official Source](https://www.virtualbox.org)
3. Reboot

## Installation

* Navigate to a directory of your choice and do

```ssh
git clone https://github.com/edwint88/plutus-vm plutus-vm
```

* Navigate to inner-folder called vagrant.

```ssh
cd plutus-vm/vagrant/
```

* In the Vagrantfile directory run & wait for provisioning to finish:

```ssh
vagrant up
```

* When it finished provisioning, open TWO terminal windows and run in each:

```ssh
vagrant ssh
```

* In Terminal Window 1:

```ssh
cd /home/vagrant/plutus/git/plutus
nix-shell
cd plutus-playground-client
plutus-playground-server
```

* In Terminal Window 2:

```ssh
cd /home/vagrant/plutus/git/plutus
nix-shell
cd plutus-playground-client
npm run start
```

* Go in your browser at https://192.168.5.21:8009 (if you changed the VM IP go to that IP)
* How to SSH-Remote: https://medium.com/@lopezgand/connect-visual-studio-code-with-vagrant-in-your-local-machine-24903fb4a9de

## Update and recompile Plutus-playground for a running VM 
Go to `cd /home/vagrant/plutus/git/plutus` => run `git pull` => run update `/bin/bash ../updatePlutus.sh`

## Tips & Tricks

* If you need ghc or cabal you can enter a folder with `default.nix` and run `nix-shell` over there and then change to your directory where you need to run cabal. E.g. `cd /home/vagrant/plutus/git/plutus` => `nix-shell` => `cd ../plutus-pioneer-program/code/week1` => `cabal build`
* If you want to use auto-complete function, then enter `nix-shell` as above and then run `vim`. (This doesn't work with `vscode`). You could open a Terminal window in `vscode` and run `vim` from there.


## Credits
Original from: [edwint88](https://github.com/edwint88/plutus-vm)

Go give him a star!
