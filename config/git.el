(require 'magit)
(require 'git-gutter+)
(require 'git-gutter-fringe+)

(global-git-gutter+-mode)

(set-face-foreground 'git-gutter+-modified "yellow")
(set-face-foreground 'git-gutter+-added    "green")
(set-face-foreground 'git-gutter+-deleted  "red")

(setq magit-last-seen-setup-instructions "1.4.0")

(add-hook 'magit-blame-file-on (lambda() (turn-off-fci-mode)))
(add-hook 'magit-blame-file-off (lambda() (turn-on-fci-mode)))
