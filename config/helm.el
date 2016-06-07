(use-package helm
  :ensure t
  :config (progn (require 'helm-config)

                 (helm-mode 1)
                 (setq helm-quick-update t)
                 (setq helm-X-x-fuzzy-match t)
                 (setq helm-buffers-fuzzy-matching t)
                 (setq helm-recentf-fuzzy-matching t)
                 (setq helm-move-to-line-cycle-in-source t)
                 (setq helm-candidate-number-limit 30)
                 (helm-autoresize-mode 1)
                 (helm-occur-init-source)
                 (helm-attrset 'follow 1 helm-source-occur)

                 (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
                 (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)

                 (require 'helm-fzf)

;                 (add-hook 'after-init-hook '(lambda()
;                                               (find-file helm-find-files nil remap) ; Replace find-file with helm-find-files
;                                               (execute-extended-command helm-M-x nil remap))) ; Replace execute-command with helm-M-x
                 (defun gears/helm-find-files-navigate-back (orig-fun &rest args)
                   "Go up one directory when pressing backspace in a directory when no additional"
                   "Characters have been entered."
                   (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
                       (helm-find-files-up-one-level 1)
                     (apply orig-fun args)))
                 (advice-add 'helm-ff-delete-char-backward :around #'gears/helm-find-files-navigate-back)))

(use-package fzf
  :ensure t)

(use-package helm-flycheck
  :ensure t)

;; Helm-Dash
(use-package helm-dash
  :ensure t
  :config (progn (setq helm-dash-browser-func 'eww)
                 (setq helm-dash-docsets-path "~/.emacs.d/docsets")
                 (setq helm-dash-enable-debugging nil)

                 (defvar my/helm-dash-official-docset-list '("C++"
                                            "C"
                                            "Emacs Lisp"
                                            "CMake"
                                            "Bash"
                                            "Python 3"))

                 (dolist (docset my/helm-dash-official-docset-list)
                   ;; Install missng docsets
                   (unless (file-exists-p (concat "~/.emacs.d/docsets/" docset ".docset"))
                     (helm-dash-install-docset docset)))))
