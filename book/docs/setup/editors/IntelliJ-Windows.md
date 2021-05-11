# IntelliJ IDEA setup on Windows for Plutus

### Credits
- https://github.com/ganeshnithyanandam
- @ganesh#9022

### My review
I have been trying to figure out an optimal IDE setup for working with Plutus smart contracts. I have come to like the following setup on Windows, using the IntelliJ IDE, its HaskForce plugin and Cygwin. All in all, a fairly good IDE experience. 

There are some trade-offs, which you can assess from the pros and cons given below.

Pros: 
   - Does not need WSL on Windows (WSL is still needed if you want to launch plutus playground due to its dependency on nix-shell) 
   -  Haskell syntax highlighting
   - View and navigate to function definitions
   - One IDE experience if you work on IntelliJ for other projects

Cons:
   - Code completion is limited to the same file. _(Maybe there is a workaround for this.)_
   - Certain sections of code causes navigation not work for all code after it in the file. eg: Saw this happen in Value.hs 
 

### Setup steps
1. Install ghc 8.10.4 from [here](https://www.haskell.org/ghc/download_ghc_8_10_4.html#windows64)
   
   Skip your OS drive if you want to space there
   

2. Install [cabal](https://www.haskell.org/cabal/download.html).
   
   Skip your OS drive if you want to space there
   

3. Add cabal and ghc to path (system variables).
   Go to `Control Panel -> System -> Advanced system settings -> Environment Variables -> Edit` to do that.


4. Install **IntelliJ IDEA** in case you do not have it already, from [here](https://www.jetbrains.com/idea/download/#section=windows)
   Community edition should work fine.
   

5. Install **Cygwin**.
   
   Cygwin is needed since without a unix like environment, running `cabal repl` will throw error:
   
   ```shell
    ...cabal.exe: The package has a ‘./configure’ script. This requires a Unix compatibility toolchain such as MinGW+MSYS or Cygwin.
   ``` 
   You just need to install CygWin and then make sure the binaries are in your path. If you install cygwin to `c:\cygwin` then the binaries are in `c:\cygwin\bin`. To add `c:\cygwin\bin` to your path temporarily you can use a command prompt and do set `path=%path%;c:\cygwin\bin`. 
      Or you can add to the path permanently like in step #3.


6. Two ways to launch the cygwin terminal.
      
   a. Run the commands below on the usual IntelliJ terminal.
   
   b. Better way:
   Add the line below line to
        `Settings(Ctrl+Alt+S) -> Tools -> Teminal -> shell path`, so that you get the cygwin shell by default when you start a terminal in IntelliJ

   _Note: Copy as is keeping the quotes_
   
      ``` shell
      "{path to your cygwin}\bin\bash" -c "exec /usr/bin/env INTELLIJ=true $SHELL --login -i"
   ```
   Upon launch of terminal use this command to switch the project directory

   ``` shell
   ${INTELLIJ-false} && cd ${OLDPWD-.}
   ```
7. Check out plutus project from SVN or load from disk.

If your project is built using cabal
8. Run cabal repl on the cygwin terminal opened in step 6.

9. After dependencies are downloaded run module discovery from `Tools | Discover Cabal Packages`


Cheers!
