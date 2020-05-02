# My emacs config
__This is a Clojure, Python and JavaScript friendly emacs config__

This repo was originally forked from the [emacs for clojure repo](https://github.com/flyingmachine/emacs-for-clojure). 


## Customizations
### UI
* Menu bar is turned off
* Show line numbers
* Graphical toolbar is removed
* Dark theme
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

... and more. Have a look at the files in the [customizations](./customizations/) folder.

## Packages

### add-node-modules-path
Searches the current files parent directories for the `node_modules/.bin/' directory and adds it to the buffer local `exec-path'. This allows Emacs to find project based installs of e.g. eslint.

### blacken
Blacken uses black to format a Python buffer.  It can be called explicitly on a certain buffer, but more conveniently, a minor-mode 'blacken-mode' is provided that turns on automatically running black on a buffer before saving.

### cider
Provides a Clojure interactive development environment for Emacs, built on top of nREPL.

### clojure-mode
Provides font-lock, indentation, navigation and basic refactoring for the Clojure programming language (http://clojure.org).

### clojure-mode-extra-font-locking
Provides additional font-locking for clojure-mode.

### color-theme-sanityinc-tomorrow
These five color themes are designed for use with Emacs' built-in theme support in Emacs 24. However, they also work with older Emacs versions, in which case color-theme.el is required.

### company
Company is a modular completion framework.  Modules for retrieving completion candidates are called backends, modules for displaying them are frontends.

### dumb-jump
Dumb Jump is an Emacs "jump to definition" package with support for 40+ programming languages that favors "just working" over speed or accuracy.

### editorconfig
EditorConfig helps developers define and maintain consistent coding styles between different editors and IDEs.

### elpy
The Emacs Lisp Python Environment in Emacs

### exec-path-from-shell
Allows environment variables to be retrieved from the shell, so that Emacs will see the same values you get in a terminal.

### flycheck
On-the-fly syntax checking for GNU Emacs 24.

### flycheck-clj-kondo
This package integrates clj-kondo with Emacs via flycheck.

### ido-completing-read+
If you use the excellent `ido-mode' for efficient completion of file names and buffers, you might wonder if you can get ido-style completion everywhere else too.

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

### projectile
Manage and navigate projects in Emacs easily

### rainbow-delimiters
Rainbow-delimiters is a "rainbow parentheses"-like mode which highlights parentheses, brackets, and braces according to their depth. Each successive level is highlighted in a different color. This makes it easy to spot matching delimiters, orient yourself in the code, and tell which statements are at a given level.

### smex
M-x interface with Ido-style fuzzy matching.

### tagedit
A collection of paredit-like functions for editing in html-mode.

### web-mode
Major mode for editing web templates.
