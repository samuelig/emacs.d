# emacs.d

This is a copy of the emacs.d/ setup I use for C/C++ development.

It installs the following packages from MELPA repository.

* ccls
* company
* company-jedi
* company-glsl
* company-lsp
* cscope
* doom-modeline
* doom-themes
* editorconfig
* elpy
* flycheck
* helm
* lsp-ui
* magit-todos
* markdown-mode
* minimap
* multiple-cursors
* pdf-tools
* projectile
* speedbar
* use-package
* treemacs-projectile
* virtualenvwrapper
* webpaste
* xcscope

![Emacs screenshot](https://raw.githubusercontent.com/samuelig/emacs.d/master/emacs-screenshot.png)

## Dependencies

It needs some system packages to work:

* ```markdown``` for compiling markdown files.
* ```bear``` for creating ```compile_commands.json``` files which are consumed by ccls for projects using make.
* ```clang``` used by company-lsp if ccls is not present.
* ```cscope```
* ```python-jedi```, ```python3-jedi```, ```virtualenv``` for python.
* ```ccls``` as C/C++/ObjC language server (for LSP).

For Debian:

```
$ sudo apt install markdown bear clang python3-jedi virtualenv cscope ccls emacs
```

For Fedora:

```
$ sudo dnf install perl-Text-Markdown bear clang python3-jedi virtualenv ccls cscope emacs
```

## Installation

To install it:

```git clone https://github.com/samuelig/emacs.d.git .emacs.d/```

And run emacs. It will automatically install everything.

## Post-installation

There are some commands to run after emacs finished installing the packages:

```M-x all-the-icons-install-fonts```

```M-x jedi:install-server```

## Notes

* Create ```compile_commands.json``` files for all the projects in the root folder (or create symlinks). See more [info](https://sarcasm.github.io/notes/dev/compilation-database.html).
   * [Cmake](https://sarcasm.github.io/notes/dev/compilation-database.html#cmake), [Ninja](https://sarcasm.github.io/notes/dev/compilation-database.html#ninja), [Bear](https://sarcasm.github.io/notes/dev/compilation-database.html#bear-and-intercept-build).

* Added keybinding ```M-X``` to set working directory to apply other command. Very useful to execute ```M-x compile```.
* Added support for opening encrypted (*.gpg) files.
* ```<F8>``` toggles treemacs window.
* ```<F9>``` toggles speedbar window.
* ```<F10>``` toggles minimap window.
* Webpaste:
  * ```C-c C-p C-r``` send region to dpaste.org.
  * ```C-c C-p C-b``` send buffer to dpaste.org.
* ```S-TAB``` inserts TAB inconditionally (specially useful for indexing code).
* ```M-.``` looks for code definitions, using ccls as backend (C/C++ projects).
* ```M-?``` finds code references, using ccls as backend (C/C++ projects).
* ```M-<mouse-1>``` adds new cursor (multiple-cursors). There are more keybindings in the init.d related to multiple-cursors package.
* There are also keybindings for org-mode. TBD.
