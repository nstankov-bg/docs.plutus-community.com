# Working on contracts with and without cabal build

### Do NOT waste time building Cabal locally. It's provided in Nix.

_**Note:**_
**The setup assumes that you are running this from an already configured
Nix-Shell, look at the [other guides](http://docs.plutus-community.com/) to set
this up.**


## Working on the plutus-pioneers-program exercises

Let's say that I have done a git clone on 2 repositories.

1. [Plutus](https://github.com/input-output-hk/plutus)
2. [Plutus-Pioneer-Program](https://github.com/input-output-hk/plutus-pioneer-program)

Both of them, I have decided to put in ```/opt/``` for the example.

The Plutus Pioneer Program source is made to compile and run against Plutus for
a specific commit hash which is listed in each week's `cabal.project` file.

For example, in `plutus-pioneer-program/code/week02/cabal.project` there's a
block like this specifying the `plutus` repo.

```bash
source-repository-package
  type: git
  location: https://github.com/input-output-hk/plutus.git
  subdir:
    freer-extras
    ...
    prettyprinter-configurable
  tag: 3746610e53654a1167aeb4c6294c6096d16b0502
```

You'll want to cd to the plutus directory and checkout the commit where the
`tag:` line is pointing before building and starting the playground. Stop the
client and server and then

```bash
git checkout 3746610e53654a1167aeb4c6294c6096d16b0502
nix build -f default.nix plutus.haskell.packages.plutus-core
```

And then start the server and client back up again. For more details and
environments other than Linux, see the [other
guides](http://docs.plutus-community.com/).

Strictly speaking, it's not necessary to `cabal build` the
plutus-pioneer-program sources at all. At this time they can't be run outside
of the simulator web interface we got set up in a different step.

When working on one, you need to copy the contents of the source file into the
Simulator Editor page's big edit control. This is the only we can run them for
now.


## Doing some of the work in a shell with cabal

It may be desireable to edit and build outside the playground because

1. You have a lot of code to write and prefer your real editor
2. You'd like to compile as you work out there from the shell or your IDE to check it
3. You may want to use the `repl` to exercise some types and/or functions
4. You may have started using unit testing on some of the code
5. You'd like to periodically commit changes to source control

First, we bring up a nix-shell which sets up the environment and has working
versions of tools (like cabal)

```ssh
cd /opt/plutus
nix-shell
```

From here we can use this nix-shell to work on the source outside the playground

```ssh
cd /opt/plutus-pioneer-program/code/week02/
cabal update  # Only required the first time and once in a while, similar to `apt update`
cabal build
```

What this `cabal build` is doing is compiling the week02 source code locally.
These build artifacts are not used by the plutus playground and the copy/paste
procedure described above is still needed to run it.
