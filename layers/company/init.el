(defun gears-layers/company-init()
  (add-hook 'after-init-hook 'global-company-mode)

  (global-company-mode 1)

  (setq company-auto-complete 'ignore)
  (setq company-auto-complete-chars nil)
  (setq company-backends '(company-c-headers company-bbdb company-nxml company-css company-cmake company-capf
                                             (company-dabbrev-code company-gtags company-keywords)
                                             company-oddmuse company-files company-dabbrev))
  (setq company-continue-commands
        '(not save-buffer save-some-buffers save-buffers-kill-terminal save-buffers-kill-emacs fixup-whitespace))
  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 0)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t))

(defun gears-layers/company-description()
  "Returns the description of the company layer."

  (eval "Provides autocompletion."))

(defun gears-layers/company-install()
  (eval t))

(defun gears-layers/company-remove()
  (eval t))

(gears-layer-defdepends company
                        :packages '(company))
