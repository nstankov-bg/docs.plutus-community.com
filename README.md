# Plutus Community Documentation.
Please feel free to [contribute](http://github.com/nstankov-bg/docs.plutus-community.com).

Future ideas for the project are shared on [Project Catalyst](https://cardano.ideascale.com/a/dtd/Incentive-system-for-documentation/352181-48088).

# Intro

Hey! The documentation for Plutus is pretty lacking so far, so we decided to make our own, as a community :)

Please feel free to commit any .MD files linked to setup or your studies. We'll figure out where to put them, so that they are not scattered around Discord.

The website is hosted on a single node, that runs in a utility-room, somewhere. Have mercy over its soul.
The page lives [here](http://docs.plutus-community.com)

# Latest working commit
As Plutus is currently being developed, so commit hashes, libraries and exercises from pioneer plutus program become outdated. As you'll see the commit hash  from plutus repo used in every lesson is so much important in order to compile or build.

Here you find a list of the latest and stable commits. Obviously, this commits maybe doesn't work with all lessons because there could be any changes on libraries.

Commit  |   Commit date | Tested in
--- |   --- |   ---
547e22a975d9b688ad2121f56e63691bead661cc    |   September 7th, 2021 |   Lesson 10, Plutus playground

# cabal.project file

```cabal.project``` file is also very important to compile successfully any lesson. So you should have the right commit from plutus repo (previous section) and your file should be configured in the right way.

Instructions here from Antonio Ibarra of IOHK:

> Oh when you update the Plutus Hash in the cabal.project file, you will also need to update the hash of all source repositories. To do that you can go to the cabal.project original repository in the defined commit (in this case https://github.com/input-output-hk/plutus/blob/9ed85d6cc8e817270220f263a11e738135eb0ad3/cabal.project),  copy from line 39 to the end of the file and then paste it in the cabal.project file of the week 10 replacing old source repositories below the note The following sections are copied from the 'plutus' repository ...

Version 0.0.16-rc2
