# Emacs

_This guide assumes that you've completed the steps described in [Prerequisites](./prerequisites.md)._

1. If you're using `nix-shell`, make sure you launch your editor from the command line so it can inherit `$PATH`
2. Install `haskell-mode` via `M-x RET package-install RET haskell-mode RET`
3. Install `lsp-mode` via `M-x RET package-install RET lsp-mode RET`
4. Install `lsp-haskell` via `M-x RET package-install RET lsp-haskell RET`
5. Edit your `.emacs` file
   ```elisp
   (require 'lsp)
   (require 'lsp-haskell)
   ;; Hooks so haskell and literate haskell major modes trigger LSP setup
   (add-hook 'haskell-mode-hook #'lsp)
   (add-hook 'haskell-literate-mode-hook #'lsp)
   ```
6. Close the editor and re-open it in the project folder
   ```bash
   cd <your-projects-dir>
   emacs .
   ```
   _Note: if you're using `nix-shell`, make sure to run it first._

If everything is fine you should get auto-completion and other features working 🎉