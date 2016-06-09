(use-package fuzzy
  :ensure t)
(use-package yasnippet
  :ensure t)

(set-default 'semantic-case-fold t)

(use-package smartparens
  :ensure t
  :config (progn (require 'smartparens-config)
                 (smartparens-global-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company
(use-package company
  :ensure t
  :config (progn (add-hook 'after-init-hook 'global-company-mode)
  
                 (global-company-mode 1)

                 (setq company-auto-complete (quote ignore))
                 (setq company-auto-complete-chars nil)
                 (setq company-backends
                       (quote
                        (company-c-headers company-ycmd company-bbdb company-nxml company-css company-cmake company-capf
                                           (company-dabbrev-code company-gtags company-keywords)
                                           company-oddmuse company-files company-dabbrev)))
                 (setq company-continue-commands
                       (quote
                        (not save-buffer save-some-buffers save-buffers-kill-terminal save-buffers-kill-emacs fixup-whitespace)))
                 (setq company-frontends
                       (quote
                        (company-pseudo-tooltip-unless-just-one-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend)))
                 (setq company-idle-delay 0)
                 (setq company-minimum-prefix-length 1)
                 (setq company-selection-wrap-around t)
                 (setq company-tooltip-align-annotations t)
                 (setq company-ycmd-show-completion-kind t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YCMD
(use-package ycmd
  :ensure t
  :config (progn (global-ycmd-mode) ; Enable ycmd for all compatible modes

                 (if (eq system-type 'darwin)
                     (set-variable 'ycmd-server-command '("/usr/bin/python" "/Users/andre/.emacs.d/ycmd-mac/ycmd"))
                   (set-variable 'ycmd-server-command '("python3" "/home/andre/.emacs.d/ycmd-linux/ycmd")))

                 (setq ycmd-extra-conf-handler 'load)

                 (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)))

(use-package company-ycmd
  :ensure t
  :config (progn (company-ycmd-setup)
                 (add-to-list 'company-backends 'company-ycmd)))

;(use-package ycmd-eldoc
;  :ensure t
;  :config (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))

;(use-package)

(use-package company-quickhelp
  :ensure company
  :config (progn (setq company-quickhelp-delay 0.1)
                 (add-hook 'prog-mode-hook '(lambda ()
                                              (unless (equal 'major-mode "emacs-lisp-mode")
                                                (company-quickhelp-mode 1))))))
