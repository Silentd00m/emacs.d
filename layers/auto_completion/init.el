(use-package company
  :ensure t
  :init (setq company-auto-complete 'ignore
              company-auto-complete-chars nil
              company-backends '(company-capf company-cmake company-keywords company-files company-dabbrev)
              company-continue-commands '(not save-buffer save-some-buffers save-buffers-kill-terminal save-buffers-kill-emacs fixup-whitespace)
              company-frontends '(company-pseudo-tooltip-unless-just-one-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend)
              company-idle-delay 0
              company-minimum-prefix-length 0
              company-selection-wrap-around t
              company-tooltip-align-annotations t)
  :diminish 'company-mode
  :config (progn (global-company-mode 1)
                 (diminish 'company-mode)))

;;(use-package company-box
;;  :ensure t
;;  :hook (company-mode . company-box-mode)
;;  :diminish 'company-box-mode)

;;(use-package company-quickhelp
;;  :ensure t
;;  :hook (company-mode . company-quickhelp-mode))
