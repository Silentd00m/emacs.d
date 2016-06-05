;;;; Sessions
(desktop-save-mode 1)

;; Save place in buffer
(use-package saveplace
  :ensure t
  :config (progn (setq-default save-place t)
                 (setq save-place-file "~/.emacs.d/saved-places")
                 (setq save-place-forget-unreadable-files nil)))
