# Docker Compose Plutus Playground Setup

## Credits

[Matthew K](https://github.com/maccam912)

## Prerequisites

1. Docker or Podman
2. docker-compose or podman-compose
3. A host system with at least:
  a. 8 GB memory
  b. 50 GB disk

note: I will use `docker` here, but `podman` can be used in every instance you see `docker`, while `podman-compose` can be used instead of `docker-compose`.
I use Fedora 33 where I can just run `sudo dnf install podman podman-compose` and be ready to rock n' roll.

## Installation

* Navigate to a directory of your choice and do

```ssh
git clone https://github.com/maccam912/ppp
```

* Navigate the new `ppp` folder

```ssh
cd ppp
```

* use `docker-compose` to build the images from the Dockerfiles

```ssh
docker-compose build
```

note: this step takes a long time, and uses up 50 GB or so. Let it run to completion.

* When docker-compose is done, start up the playground!

```ssh
docker-compose up
```

You should now be able to connect to `https://localhost:8009` (_Note the S on HTTPS, this uses a self-signed certificate!_) and after accepting the warning, get to a playground!


## Tips & Tricks

I am running on Hetzner Cloud for a cheap 8 GB RAM instance. After the initial `docker-compose build` I made a snapshot I can restore from to shut down the instance and save money when I don't need the playground.
