# Windows Plutus Setup with WSL2 and nix

## Credits
[Nick Ayotte](https://github.com/nicolasayotte)

## Notes

Should work on Windows 10 with WSL 2

## Building plutus and a dev container

1 - Get WSL2 on Windows

2 - Get a normal distro (Ubuntu or Debian work fine)

3 - In your wsl shell, in your linux ~/src (for example) do:

    [$] git clone https://github.com/input-output-hk/plutus

4 - Read the README file it will tell you how to get Nix

5 - Install Nix

    [$] curl -L https://nixos.org/nix/install | sh

6 - Configure nix by creating the file /etc/nix/nix.conf and writing into it:

    sandbox = false
    use-sqlite-wal = false
    substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
    trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=

7 - Relaunch the shell

8 - Build Plutus with the Nix command line in the README file

    [$] nix build -f default.nix plutus.haskell.packages.plutus-core.components.library

9 - Install Docker for Windows (relaunch your shell)

10 - Launch Docker and make sure it is WSL2 enabled (in the settings)

11 - Launch the Docker container from WSL using the README command line
     *Just before running docker load < $(nix-build default.nix -A devcontainer) might be that you need to checkout 
     the hash of plutus repo that is expected within the cabal project file 
     
    [$] git checkout 03a95411238225db1d10288fbd3b405f5f53c78b 
    [$] docker load < $(nix-build default.nix -A devcontainer)

## Setting up VSCode with the dev container.

References:
https://code.visualstudio.com/docs/remote/wsl#_advanced-opening-a-wsl-2-folder-in-a-container

1 - Get VSCode for Windows

2 - Get the Remote - Containers extension

3 - Go back into WSL into your source directory (~/src , maybe)

4 - git clone https://github.com/input-output-hk/plutus-starter

5 - cd plutus-starter

6 - code .
    This will install VSCode Server on your linux, wait for it
    somehow I had to shutdown my vpn for that to download, should
    not take long at all. :)

7 - VSCode will launch in Windows and ask to reopen in the Dev Container: yes.

8 - Open the CLI you will get a nifty [nix]: CLI

9 - 'cabal update' then 'cabal build' and watch it build the Haskell


## Setting up the Plutus Playground

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
    [$] plutus-playground-server
    [$] npm run start

**We're done!**

The playground should be up and running.

Open your finest browser and navigate to:

[https://localhost:8009/](https://localhost:8009/)

## Troubleshooting

1 - [https://localhost:8009/](https://localhost:8009/) Connection refused, reset or otherwise wonky.

    Modify the plutus/plutus-playground-client/package.json line in scripts so that it launches the server on 0.0.0.0 instead of 127.0.0.1:
	"webpack:server": "webpack-dev-server --host 0.0.0.0 --progress --inline --hot --mode=development --port=8009"

2 - ubuntu VPN Cisco Anyconnect unable to connect to IP resource (vscode not fetching resource in wsl/ page not loading in browser)

Changing the Interface Metric 1 -> 6000 for AnyConnect VPN Adapter resolves the connection issue, but this has to be done every time you connect the VPN.
To automate this, put the PS command in a script and created a Task to run every time there is a network change.

Save the script in a file
First, create the script. I have a 'scripts' directory in my user home, so I put it at:

    %HOMEPATH%\scripts\UpdateAnyConnectInterfaceMetric.ps1

    Get-NetAdapter | Where-Object {$_.InterfaceDescription -Match "Cisco AnyConnect"} | Set-NetIPInterface -InterfaceMetric 6000

You can save it where you want, just make sure to use that path in step 13 below.

Create the scheduled task:
1. Open 'Task Scheduler'

2. Click "Create Task" on Right Sidebar

3. Name: Update Anyconnect Adapter Interface Metric for WSL2

4. Set Security Options

    Check box: 'Run with highest priveleges'

5. Select 'Triggers' Tab

6. Click 'New' at bottom of Window

7. Open 'Begin the task' drop-down

8. Select 'On an Event'

9. Configure Event:

    Log: 'Microsoft-Windows-NetworkProfile/Operational'
    Source: 'NetworkProfile'
    Event ID: '10000'
10. Click 'OK'

11. Select 'Actions' Tab

12. Click 'New'

13. Configure Action:

    Action: 'Start a Program'
    Program/script: 'Powershell.exe'
    Add arguments: '-ExecutionPolicy Bypass -File %HOMEPATH%\scripts\UpdateAnyConnectInterfaceMetric.ps1'
14. Click 'OK'

15. Select 'Conditions' Tab

16. Uncheck box:

    Power -> Start the task only if the computer is on AC Power

17. Click 'OK'

When AnyConnect finishes connecting, a Powershell window pops up for a couple seconds and WSL can reach the network.

~This solution has been found on https://github.com/microsoft/WSL/issues/4277 



