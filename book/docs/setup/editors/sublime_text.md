# Sublime Text

### Credits
- https://github.com/tmphey
- @nymeron#8182

_This guide assumes that you've completed the steps described in [Prerequisites](./prerequisites.md)._

1. If you're using `nix-shell`, make sure you can launch the editor from the command line so it inherits your `$PATH`.
   Follow the instructions [here](https://www.sublimetext.com/docs/command_line.html).
2. Install LSP: Open the command palette and run `Package Control: Install Package`, then select `LSP`.
3. Open LSP settings: `Preferences -> Package Settings -> LSP -> Settings` and add this
   ```json
   {
    "clients": {
        "haskell-language-server": {
          "command": ["haskell-language-server", "--lsp"],
          "scopes": ["source.haskell"],
          "syntaxes": ["Packages/Haskell/Haskell.sublime-syntax"],
          "languageId": "haskell",
          "enabled": true
        },
    }
   }
   ```
4. Close the editor and re-open it in the project folder
   ```bash
   cd <your-projects-dir>
   subl .
   ```
   _Note: if you're using `nix-shell`, make sure to run it first._

If everything is fine you should get auto-completion and other features working 🎉