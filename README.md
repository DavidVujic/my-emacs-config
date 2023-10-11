# My emacs config
__This is a Clojure, Python, JavaScript and TypeScript friendly emacs config__

This repo was originally forked from the [emacs for clojure repo](https://github.com/flyingmachine/emacs-for-clojure). 


## Customizations
### UI
* Menu bar is turned off
* Show line numbers
* Graphical toolbar is removed
* Dark theme (on first run, you probably have to confirm that Emacs should be allowed to load the theme)
* Custom window size and position (customized for the machine I am using - a Dell XPS 13)
* Navigation up & down smoothly, without the annoying buffer centering
* Disabled the Emacs startup message

... and more. Have a look at the [ui.el](./customizations/ui.el) file and the other config files in the [customizations](./customizations/) folder.

### Editing
* Comment and uncomment a line or section with `C-;`
* Enable special chars in the editor, like ~ and ^.
* Move text up and down with `control shift up` and `control shift down`
* Multiple select, "mark next like this" with `C->`
* Disabled the "suspend-frame" command, too easy to press `C-z` by mistake
* Disabled the Emacs startup message
* A buffer is auto saved when losing focus (using the package `super-save`)

... and more. Have a look at the files in the [customizations](./customizations/) folder.

### Python
To enable flake8, mypy and black as soon as opening a Python file you will need to have those installed on your machine.

``` shell
 pip3 install flake8 flake8-bugbear mypy black
```

Alternatively, you can manually enable these tools after have activated a virtual environment (containing the tools) within Emacs.

#### Python shell
The shell is set to `IPython` and Emacs will expect to find it installed on your machine.

``` shell
pip3 install ipython
```

Or, change the shell setting in `customizations/setup-python.el`, according to the [elpy docs about interpreter setup](https://elpy.readthedocs.io/en/latest/ide.html#interpreter-setup).

#### Evaluating Python code, like with Clojure?
I am evaluating (pun) ways to write code in a Clojure-like interactive way.

My current setup:
* IPython as the shell.
* Evaluating code (entire buffer, region or selection) with [elpy commands](https://elpy.readthedocs.io/en/latest/ide.html#evaluating-code-fragments).
* A custom IPython config to auto-reload changed.

How to create and edit [IPython config files](https://ipython.readthedocs.io/en/stable/config/intro.html#python-configuration-files)

My custom `IPython` config (added/uncommented these rows and added values). This will enable auto-reload of modules in the shell.

``` python
c.InteractiveShellApp.extensions = ['autoreload']
c.InteractiveShellApp.exec_lines = ['%autoreload 2']
```

### Clojure
Make sure you have `clj-kondo` installed on your machine according to the [install instructions](https://github.com/clj-kondo/clj-kondo/blob/master/doc/install.md)

## Packages

### add-node-modules-path
Searches the current files parent directories for the `node_modules/.bin/' directory and adds it to the buffer local `exec-path'. This allows Emacs to find project based installs of e.g. eslint.

### auto-virtualenv
Automatically activate python virtualenvs.

### blacken
Blacken uses black to format a Python buffer.  It can be called explicitly on a certain buffer, but more conveniently, a minor-mode 'blacken-mode' is provided that turns on automatically running black on a buffer before saving.

### cider
Provides a Clojure interactive development environment for Emacs, built on top of nREPL.

### clj-refactor
Provides refactoring support for Clojure projects. It complements the refactoring functionality you'd find in clojure-mode and CIDER.

### clojure-mode
Provides font-lock, indentation, navigation and basic refactoring for the Clojure programming language (http://clojure.org).

### clojure-mode-extra-font-locking
Provides additional font-locking for clojure-mode.

### color-theme-sanityinc-tomorrow
These five color themes are designed for use with Emacs' built-in theme support in Emacs 24. However, they also work with older Emacs versions, in which case color-theme.el is required.

### company
Company is a modular completion framework.  Modules for retrieving completion candidates are called backends, modules for displaying them are frontends.

### company-jedi
company-mode completion back-end for Python JEDI.
(NOTE: I had to run `jedi:install-server` from within emacs to make this backend work)

### dockerfile-mode
Major mode for editing Dockerfiles

### dumb-jump
Dumb Jump is an Emacs "jump to definition" package with support for 40+ programming languages that favors "just working" over speed or accuracy.
Note: to be able to find and jump to re-agent events and subscriptions I have added Silversearcher according to the dumb-jump install instructions. The lecacy dumb-jump-go, however, do find those without the extra OS dependency.

### editorconfig
EditorConfig helps developers define and maintain consistent coding styles between different editors and IDEs.

### elpy
The Emacs Lisp Python Environment in Emacs.

### emojify
Display emojis in Emacs.

### exec-path-from-shell
Allows environment variables to be retrieved from the shell, so that Emacs will see the same values you get in a terminal.

### flycheck
On-the-fly syntax checking for GNU Emacs 24.

### flycheck-clj-kondo
This package integrates clj-kondo with Emacs via flycheck. Make sure you also have clj-kondo installed globally on your machine according to the official install instructions.

### graphql-mode
graphql-mode is an emacs mode to edit GraphQL schema and queries.

### ido-completing-read+
If you use the excellent `ido-mode' for efficient completion of file names and buffers, you might wonder if you can get ido-style completion everywhere else too.

### py-isort
Provides commands, which use the external isort tool to tidy up the imports in the current buffer.
Note: Install isort on your machine `pip install isort`.

### js2-mode
Improved JavaScript editing mode.

### js-comint
Run a JavaScript interpreter in an inferior process window.

### json-mode
Major mode for editing JSON files. Extends the builtin js-mode to add better syntax highlighting for JSON and some nice editing keybindings.

### live-py-mode
Visualize your Python code while you type it in Emacs. Activate it with M-x `live-py-mode`.

### magit
Magit is an interface to the version control system Git, implemented as an Emacs package.

### markdown-mode
Major mode for Markdown-formatted text.

### move-text
Allows you to move the current line using M-up / M-down if a region is marked, it will move the region instead.

### multiple-cursors
Multiple cursors for Emacs. This is some pretty crazy functionality, so yes, there are kinks. Don't be afraid though, I've been using it since 2011 with great success and much merriment.

### pandoc-mode
Minor mode for interacting with Pandoc.

### paredit
Minor mode for editing parentheses.

### pipenv
A Pipenv porcelain inside Emacs.

### polymode
A framework for multiple major modes (MMM) inside a single Emacs buffer.

I use this mode to get syntax highlighting for inline SQL in Python files. Activate the mode with M-x poly-python-sql-mode.

### prettier
js/ts prettier mode

### projectile
Manage and navigate projects in Emacs easily

### pyenv-mode
Add support for pyenv.

### rainbow-delimiters
Rainbow-delimiters is a "rainbow parentheses"-like mode which highlights parentheses, brackets, and braces according to their depth. Each successive level is highlighted in a different color. This makes it easy to spot matching delimiters, orient yourself in the code, and tell which statements are at a given level.

### smex
M-x interface with Ido-style fuzzy matching.

### super-save
super-save auto-saves your buffers, when certain events happen - e.g. you switch between buffers, an Emacs frame loses focus, etc.
You can think of it as both something that augments and replaces the standard auto-save-mode.

### tagedit
A collection of paredit-like functions for editing in html-mode.

### terraform-mode
Major mode for terraform configuration files.

### tide
TypeScript interactive development environment for Emacs.

### typescript-mode
TypeScript specific mode, using it in combination with tide.

### treemacs
A tree layout file explorer for Emacs.

### web-mode
Major mode for editing web templates.

### which-key
Minor mode for Emacs that displays the key bindings following your currently entered incomplete command (a prefix) in a popup.

### yaml-mode
Major mode for editing YAML files.
