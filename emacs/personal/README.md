# My Emacs configuration on top of Prelude

1. Emacs taught me freedom for software
2. Emacs taught me how to read code
3. Emacs taught me power of Lisp
4. Emacs taught me how to implement a language core
5. Emacs taught me how to implement a garbage collector
6. Emacs helped me to code and debug
7. Emacs helped me to write and edit text/mails/documents
8. Emacs helped me to be a effective programmer
9. Emacs made me a hacker
10. Emacs has changed my life forever

_-- Yukihiro "matz" Matsumoto_


In `prelude-modules.el` are the selected Prelude modules I need. In `personal/` are the thing I configured on top of Prelude's wonderful defaults.



## TODOs

* Simple webbrowsing from Emacs (Chris Doner exmplains)
* Full Haskell setup for Emacs
* Get all the Ruby/Rails tricks described in a section of this file (currently stuck at `bundle exec` support)
* [Multiple cursors with Evil](https://github.com/mbriggs/.emacs.d/blob/master/init/init-multiple-cursors.el)
* Access email (GMail for now) from Emacs -- using `wanderlust` maybe?
* Use SoundCloud from Emacs
* Try out ChucK (realtime strongly typed music programming), and/or SuperCollider


## Shortcuts I seem to forget

Window (pane) management:
* `C-x 0` -- kill current pane
* `C-x 1` -- hide all not current panes
* `C-x 2` -- split horizonally
* `C-x 3` -- split vertically
* `C-c s` -- swap two panes (needs exactly two panes)
* `C-x o` and `C-x O` -- cycle current pane
* `C-c o` -- make minibuffer the current pane
* `S-<cursor>` -- move current pane (from the `windmove` package)
* `C-(shift-)<TAB>` -- visual "Alt-TAB" style buffer switching (from the `iflipb` package)

Commenting with the nerd-commenter port:
* `M-;` -- toggle comment on line(s) (works in all modes/states)
* `,ci` -- toggle comment on line(s) [normal]
* `,cc` -- insert a commented copy of the current line above it and move the cursor to the end of the current line [normal]
* `,cp` -- toggle comment on paragraph [normal]
* `,cr` -- toggle commment on region [normal]

Cursor movement:
* `H`, `M`, `L` -- top, middle and bottom of screen [normal]
* `e`, `w`, `b`, `E`, `W` and `B` -- move by word (w/ and w/o punctuation) [normal]
* `%` -- move to related item (e.g.: different types of braces) [normal]
* `f`, `t`, `F`, `T` and `;` -- go before (`fF`) or on (`tT`) the previous (`FT`) or next (`ft`) occurence of a char and `;` to repeat it
* `SPC` -- ACE jump char mode [normal]
* `C-c c` -- ACE jump char mode [normal]
* `C-c w` -- ACE jump word mode [normal]
* `C-c l` -- ACE jump line mode [normal]

Emacs style operations:
* `C-backspace` -- kill line backwards from cursor
* `C-\` -- align on regexp (nice to use when selecting a region)

Search and replace within a buffer:
* Do it like in Vim (`:%s/orig/target/g` or `/search`)
* `C-o` during a search to list the occurence in a pane

Buffer management in IBuffer mode (`C-x C-b`):
* `RET` -- open the buffer
* `m` -- flag a buffer as "marked"
* `d` -- flag current buffer for deletion
* `C-d` -- flag current buffer for deletion and move up
* `D` -- immediately delete buffer
* `x` -- execute flags
* `u` -- unflag current buffer
* `U` -- replace regexp in all marked buffers
* `Q` -- query replace (by comfirmation) in all marked buffers
* `o` -- open buffer in pane on the right (preview)

Buffer management without IBuffer mode
* `C-x b` -- switch buffer by ido
* `C-c J` -- switch buffer by ACE jump
* `C-c h` -- swith buffer by helm mode (switches more then just buffers)
* `C-x k` -- kill buffer (followed by `RET` for current buffer)

Project management (using `projectile`):
* `Super-p` -- switch projects (anything previously opened with Projectile)
* `Super-f` -- find file in the current project
* `Super-d` -- find directory in the current project
* `Super-g` -- grep in the files of the current project (`C-c p a` for ack)
* `C-c p f` -- ido find file in project
* `C-c p b` -- ido find currently open project buffer
* `C-c p d` -- display a list of all directories in the project
* `C-c p g` -- run grep on the files in the project
* `C-c p o` -- runs multi-occur on all project buffers currently open
* `C-c p r` -- runs interactive query-replace on all files in the projects
* `C-c p i` -- invalidates the project cache (if existing)
* `C-c p R` -- regenerates the projects TAGS file
* `C-c p k` -- kills all project buffers
* `C-c p D` -- opens the root of the project in dired
* `C-c p e` -- shows a list of recently visited project files
* `C-c p a` -- runs ack on the project (requires `ack-and-a-half`)
* `C-c p c` -- runs a standard compilation command for your type of project
* `C-c p p` -- runs a standard test command for your type of project
* `C-c p z` -- adds the currently visited to the cache
* `C-c p s` -- display a list of known projects you can switch to

File tree view (using `sr-speedbar`):
* `C-x t` -- toggle file view
* `Shift-up` -- move up one dir [speedbar mode]
* `RET` -- change to dir or open file under cursor [speedbar mode]
* `SPACE` or `right` -- expand dir or show files internal structure [speedbar mode]
* `left` -- closes a node in the tree [speedbar mode]

Run in-Emacs apps:
* `C-x p` -- view processes (`proced`)
* `C-x d` -- view directory contents (`dired`)
* `C-x t` -- toggle file system as tree "drawer" (`sr-speedbar`)
* `C-x m` -- `eshell`
* `C-x M` -- start a new `eshell`
* `C-c t` -- start a new `ansi-terminal` (works well with my `zsh`)
* `C-x M-m` -- start the user's `$SHELL` (not working well with my `zsh`)
* `M-x erc` -- start ERC (IRC for Emacs); channels are buffers

Interfacing Git (using `magit`):
* `Super-m m` and `C-x g` -- open Magit status buffer (see this [cheatsheet](http://daemianmack.com/magit-cheatsheet.html) for further commands)
* `Super-m l` -- open Magit log buffer
* `Super-m f` -- open Magit file log buffer
* `Super-m b` -- toggle Magit in-line blame log

Interesting commmands:
* `M-x undo-tree-visualize` -- show a browsable undo tree
* `M-x browse-kill-ring` -- browse all recently deleted pieces of text
