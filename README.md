# emacs.d

This is a copy of the emacs.d/ setup I use for C/C++ development.

It installs the following packages from MELPA repository.

* ccls
* company-irony
* company-irony-c-headers
* company-jedi
* company-lsp
* counsel
* doom-modeline
* doom-themes
* editorconfig
* elpy
* flycheck
* flycheck-irony
* glsl-mode
* irony
* irony-eldoc
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

![Emacs screenshot](https://raw.githubusercontent.com/samuelig/emacs.d/master/emacs-screenshot.png)

## Dependencies

It needs some system packages to work:

* ```markdown``` for compiling markdown files.
* ```bear``` for creating ```compile_commands.json``` files which are consumed by irony.
* ```clang``` used by irony.
* ```GNU global``` used by ggtags.
* ```exuberant-ctags``` used by ggtags.
* ```python-jedi```, ```python3-jedi```, ```virtualenv``` for python.
* ```ccls``` as C/C++/ObjC language server (for LSP).

For Debian:

```
$ sudo apt install markdown bear clang clang-dev exuberant-ctags python-jedi python3-jedi virtualenv ccls
```

For Fedora:

```
$ sudo dnf install perl-Text-Markdown bear clang clang-devel ctags-etags python-jedi python3-jedi virtualenv ccls
```

## Installation

To install it:

```git clone git@github.com:samuelig/emacs.d.git .emacs.d/```

And run emacs. It will automatically install everything.

## Post-installation

There are some commands to run after emacs finished installing the packages:

```M-x irony-install-server```

```M-x all-the-icons-install-fonts```

```M-x jedi:install-server```
