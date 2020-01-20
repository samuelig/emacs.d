# emacs.d

This is a copy of the emacs.d/ setup I use for C/C++ development.

It installs the following packages from MELPA repository.

* ccls
* company-irony
* company-lsp
* counsel
* doom-modeline
* doom-themes
* editorconfig
* elpy
* flycheck
* ivy
* lsp-ui
* magit-todos
* markdown-mode
* multiple-cursors
* pdf-tools
* projectile
* speedbar
* swiper
* use-package
* virtualenvwrapper
* webpaste

![Emacs screenshot](https://raw.githubusercontent.com/samuelig/emacs.d/master/emacs-screenshot.png)

## Dependencies

It needs some system packages to work:

* ```markdown``` for compiling markdown files.
* ```bear``` for creating ```compile_commands.json``` files which are consumed by ccls.
* ```clang``` used by irony.
* ```GNU global``` used to generate TAGS.
* ```exuberant-ctags``` used to generate TAGS.
* ```python-jedi```, ```python3-jedi```, ```virtualenv``` for python.
* ```ccls``` as C/C++/ObjC language server (for LSP).

For Debian:

```
$ sudo apt install markdown bear clang exuberant-ctags global python-jedi python3-jedi virtualenv ccls
```

For Fedora:

```
$ sudo dnf install perl-Text-Markdown bear clang ctags-etags global python-jedi python3-jedi virtualenv ccls
```

## Installation

To install it:

```git clone git@github.com:samuelig/emacs.d.git .emacs.d/```

And run emacs. It will automatically install everything.

## Post-installation

There are some commands to run after emacs finished installing the packages:

```M-x all-the-icons-install-fonts```

```M-x jedi:install-server```

## Notes

* Create ```compile_commands.json``` files for all the projects in the root folder (or create symlinks). See more [info](https://sarcasm.github.io/notes/dev/compilation-database.html).
   * [Cmake](https://sarcasm.github.io/notes/dev/compilation-database.html#cmake), [Ninja](https://sarcasm.github.io/notes/dev/compilation-database.html#ninja), [Bear](https://sarcasm.github.io/notes/dev/compilation-database.html#bear-and-intercept-build).

* Added keybinding ```M-X``` to set working directory to apply other command. Very useful to execute ```M-x compile```.
* Added support for opening encrypter (*.gpg) files.
* ```<F8>``` launches speedbar.
* ```M-.``` looks for code definitions, using ccls as backend. It uses ctags as a fallback.
