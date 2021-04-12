# Plutus VM (Ubuntu Based) for Plutus Pioneer Program

## Credits
Original from: [edwint88](https://github.com/edwint88/plutus-vm)

Go give him a star!

## Prerequisites

1. Obtain & Install Vagrant from the [Official Source](https://www.vagrantup.com)
2. Obtain & Install VirtualBox from the [Official Source](https://www.virtualbox.org)
3. Reboot

## Installation

1. Navigate to a directory of your choice and do

```ssh
git clone https://github.com/edwint88/plutus-vm plutus-vm
```

2. Navigate to inner-folder called vagrant.

```ssh
cd plutus-vm/vagrant/
```

3. In the Vagrantfile directory run & wait for provisioning to finish:

```ssh
cd vagrant && vagrant up
```

4. When it finished provisioning, open TWO terminal windows and run in each:

```ssh
vagrant ssh
```

5. In Terminal Window 1:

```ssh
cd /home/vagrant/plutus/git/plutus
nix-shell
cd plutus-playground-client
plutus-playground-server
```

6. In Terminal Window 2:

```ssh
cd /home/vagrant/plutus/git/plutus
nix-shell
cd plutus-playground-client
npm run start
```

7. Go in your browser at https://192.168.5.21:8009 (if you changed the VM IP go to that IP)
8. How to SSH-Remote: https://medium.com/@lopezgand/connect-visual-studio-code-with-vagrant-in-your-local-machine-24903fb4a9de

## Tips & Tricks

1. If you need ghc or cabal you can enter a folder with `default.nix` and run `nix-shell` over there and then change to your directory where you need to run cabal. E.g. `cd /home/vagrant/plutus/git/plutus` => `nix-shell` => `cd ../plutus-pioneer-program/code/week1` => `cabal build`


## Credits
Original from: [edwint88](https://github.com/edwint88/plutus-vm)

Go give him a star!