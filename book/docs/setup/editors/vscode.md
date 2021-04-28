# Visual Studio Code

### Credits
- https://github.com/tmphey
- @nymeron#8182
- @getHashSet

_This guide assumes that you've completed the steps described in [Prerequisites](./prerequisites.md)._

1. If you're using `nix-shell`, make sure you can launch the editor from the command line so it inherits your `$PATH`.
   On Linux, `code` command is automatically available after VSCode installation.
   On macOS, follow the [instruction](https://code.visualstudio.com/docs/setup/mac#_launching-from-the-command-line)
2. Install the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
3. Close the editor and re-open it in the project folder
   ```bash
   cd <your-projects-dir>
   code .
   ```
   _Note: if you're using `nix-shell`, make sure to run it first._

If everything is fine you should get auto-completion and other features working 🎉

---

### Additional Troubleshooting Steps
_In the event the above steps result in the Visual Studio Code Editor not displaying definitions._

### How to Check
_Select a function in the Visual Studio Code Editor and right click and choose **Go to Definition** or highlight a function and press **F12**_.

`Example: bindMaybe displays the error No definition found for 'bindMabye' even though it is defined only a few lines up.`
```
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) f = f x

foo :: String -> String -> String -> Maybe Int
foo x y z =  readMaybe x `bindMaybe` \num1 ->
             readMaybe y `bindMaybe` \num2 ->
             readMaybe z `bindMaybe` \num3 ->
             Just (num1 + num2 + num3)
```

### Possible Solution
1. Open the built-in terminal inside your Visual Studio Code by pressing `control` _+_ `~ ` at the same time or by going to `Terminal > New Terminal` within the file menu.

2. From the built in terminal navigate to your **plutus** directory or clone a new copy of the [plutus repository](https://github.com/input-output-hk/plutus).

3. From the **plutus** root directory run the following command `nix-shell`. (_This command requires access to the `default.nix, shell.nix ` files that are part of every plutus repository fork or clone._)

`nix-shell Example: Should look something like this.`
```c
[nix-shell:~/<your file path>/plutus-pioneer-program/code/week04]$ |
```

4. Once in the nix-shell instance navigate back to your **plutus-pioneer-program/code/weekxx/** folder and run `code .` (_Note: This will launch a new Visual Studio Code Window. It is now safe to close the prior Visual Studio Code instance._)

5. :boom:

Your Visual Studio Code instance should now have access to definitions. Test this by selecting a function an pressing **F12**.

`Definition Example: When pressing F12 or while hovering the function name.`
```haskell
bindMaybe :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
Defined at /<your path>/plutus-pioneer-program/code/week04/src/Week04/Maybe.hs:19:1
```
