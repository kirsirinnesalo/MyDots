# MyDots

Personal dot files

## Emacs

### init.el

Emacs conf all in one file

* Define some keys
* Backups
* UI settings
* [use-package](https://github.com/jwiegley/use-package)
* [MultiMarkdown](http://fletcherpenney.net/multimarkdown/) for multiple platforms
  * [markdown-mode](https://github.com/jrblevin/markdown-mode)
* [monokai-theme](https://github.com/oneKelvinSmith/monokai-emacs)

### myline.el
Mode line configuration.
Faces are mainly copied from [Amit](http://amitp.blogspot.fi/2011/08/emacs-custom-mode-line.html).

Format:

	column status user-login-name@system-name:directory/filename [filesize] vc-mode [modes] date time (uptime)

I have `global-linum-mode` enabled in `init.el` so myline does not show line numbers.

![Myline screenshot](.emacs.d/lisp/myline-screenshot.png)

## Git

### .gitconfig

* Use `emacs` as defaut editor
* Mercurial style `in` and `out` aliases
* Pretty log

### .gitignore

Basic ignore patterns for SVN, Eclipse, Netbeans, Idea, Emacs and so on...

## Mercurial

### .hgrc

* Use `emacs` as default editor
* Lots of extensions
* Status aliases for queues

## Thunderbird

### Add-ons in use

* **Colored Diffs:** Color diffs sent by VCS
* **Extra Folder Column:** In core now
* **Mail Redirect:** Allow redirect a.k.a. bounce
* **Xpunge:** Button for emptying trash and junk and compacting folders

### user.js

* check all folders for new messages
* reply header: "On [date] [author] wrote:"
* short date format as default
* no date format today
* sort order ascending
* sort by thread
