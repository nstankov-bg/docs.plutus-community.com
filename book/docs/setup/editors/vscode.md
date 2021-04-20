# Visual Studio Code

### Credits
- https://github.com/tmphey
- @nymeron#8182

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