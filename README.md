# Typst Mode

**Note**: For Emacs 29 users, please use [typst-ts-mode](https://git.sr.ht/~meow_king/typst-ts-mode) instead, which is a lot more consummate than this mode.  
**Note2**: I suppose I have no motion and plan to update this major mode in a long time, so I'll achieve 
this major mode(github). I will spend more energy into maintaining `typst-ts-mode` and [uben0/tree-sitter-typst](https://github.com/uben0/tree-sitter-typst)  

Emacs support for [Typst](https://github.com/typst/typst). 

## Requirements
1. `typst`

## Installation

With `use-package` and `straight`:

``` emacs-lisp
(use-package typst-mode
  :straight (:type git :host github :repo "Ziqi-Yang/typst-mode.el"))
```
Manually:

``` emacs-lisp
;; add typst-mode directory to the load-path
(push (expand-file-name "modules/languages/typst-mode" user-emacs-directory) load-path)
;; load tyspt-mode
(require 'typst-mode)
```

## Provided Functions
1. `typst-compile` (default keybinding: `C-c C-c`; also provide `typst-compile-preview`)
2. `typst-preview` (default keybinding: `C-c C-p`)
3. `typst-toggle-watch` (default keybind: `C-c C-w`; also provide `typst-watch` and `typst-stop-watch`)

## Customization

Faces can be configured through `M-x customize`: `Text` -> `Typst` -> `Typst Mode Faces`.

## Contribution

This project is mainly hosted on [SourceHut](https://sr.ht/~meow_king/typst-mode/), which means you are welcome to use email as the primary contribution methods (including proposing an new idea on public mailing list and send a patch via `git send-email`). However, it is also acceptable to use GitHub.   

[Mailing lists](https://sr.ht/~meow_king/typst-mode/lists)
