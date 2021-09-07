# emacs.d

![Emacs screenshot](https://raw.githubusercontent.com/samuelig/emacs.d/master/emacs-screenshot.png)

This is a copy of the emacs.d/ setup I use for C/C++ development.

It installs the following packages from MELPA repository.

* `ccls` package for this C/C++/ObjC language server (for LSP)
* `centered-window` centers the text of the window like Zen Mode in Visual Studio Code.
* `company` text completion framework for emacs.
* `company-jedi` company backend for jedi server (python).
* `company-glsl` company backend for GLSL.
* `doom-modeline` fast mode-line for emacs.
* `doom-themes` themes for doom.
* `editorconfig` support for this coding style tool.
* `elpy` emacs python development environment (easy way to install a fully featured python dev environment).
* `flycheck` alternative to flymake. Syntax checking.
* `helm` incremental completion and narrowing selection framework.
* `lsp-ui` UI support for LSP (it shows declarations in a box)
* `magit-todos` it shows TODOs present in the repository using magit (very powerful git client for emacs).
* `markdown-mode` support for markdown.
* `minimap` it shows a minimap of the buffer like vscode.
* `multiple-cursors` adds support for multiple cursors.
* `pdf-tools` adds support to view PDFs.
* `projectile` it adds support for code projects in emacs.
* `speedbar` it displays information of the current buffer, like the functions and definitions, so you can browse easily.
* `use-package` package that facilitates the install and configuration of emacs packages.
* `treemacs-projectile` package that adds support for tree-like view of projects.
* `virtualenvwrapper` adds support for virtualenv on emacs.
* `webpaste` a package that allows to paste code to dpaste.org.
* `xcscope` adds support for cscope.
 
## Dependencies

It needs some system packages to work:

* ```markdown``` for compiling markdown files.
* ```bear``` for creating ```compile_commands.json``` files which are consumed by ccls for projects using make.
* ```clang``` used by lsp if ccls is not present.
* ```cscope``` for looking for C/C++ definitions and calls. 
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
* ```<F9>``` shows lsp-treemacs-symbols window.
* ```<F10>``` toggles minimap window.
* Webpaste:
  * ```C-c C-r``` send region to dpaste.org.
  * ```C-c C-b``` send buffer to dpaste.org.
* ```S-TAB``` inserts TAB inconditionally (specially useful for indexing code).
* ```M-i``` fuzzy search in the same buffer.
* ```M-.``` looks for code definitions, using ccls as backend (C/C++ projects).
* ```M-?``` finds code references, using ccls as backend (C/C++ projects).
* ```M-<mouse-1>``` adds new cursor (multiple-cursors). There are more keybindings in the init.d related to multiple-cursors package.
* There are also keybindings for org-mode. TBD.
