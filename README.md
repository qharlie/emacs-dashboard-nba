# emacs-dashboard-nba
Adds last nights scores and summaries for NBA games



![Screenshot](screenshot.png?raw=true "Screenshot")


Sample Config ( until I get it onto MELPA ) 

```lisp
(require 'dashboard)
(load-file "~/workspace/emacs-dashboard-reddits/dashboard-reddit.el")
(load-file "~/workspace/emacs-dashboard-stackxc/dashboard-stacxc.el")
(load-file "~/workspace/emacs-dashboard-nba/dashboard-nba.el")

(setq dashboard-items '(
			(recents  . 5)
                        (bookmarks . 5)
			(projects . 5)
			(reddits . 5)
			(nbas . 10)
			))
```
