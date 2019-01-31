# emacs.d

This is a copy of the emacs.d/ setup I use for C/C++ development.

It installs the following packages from MELPA repository.

* company-irony
* company-irony-c-headers
* company-jedi
* counsel
* doom-modeline
* doom-themes
* editorconfig
* elpy
* flycheck
* ggtags
* glsl-mode
* irony
* irony-eldoc
* ivy
* magit-gh-pulls
* markdown-mode
* multiple-cursors
* pdf-tools
* swiper
* use-package
* virtualenvwrapper
* xcscope

![Emacs screenshot](https://raw.githubusercontent.com/samuelig/emacs.d/master/emacs-screenshot.png)

## Dependencies

It needs some system packages to work:

* ```markdown``` for compiling markdown files.
* ```bear``` for creating ```compile_commands.json``` files which are consumed by irony.
* ```clang``` used by irony.
* ```GNU global``` used by ggtags.
* ```exuberant-ctags``` used by ggtags.
* ```cscope```
* ```python-jedi```, ```python3-jedi```, ```virtualenv``` for python.

```
$ sudo apt install markdown bear clang global exuberant-ctags cscope python-jedi python3-jedi virtualenv
```

## Installation

To install it:

```git clone git@github.com:samuelig/emacs.d.git .emacs.d/```

And run emacs. It will automatically install everything.

## Post-installation

There are some commands to run after emacs finished installing the packages:

```M-x irony-install-server```

```M-x all-the-icons-install-fonts```
