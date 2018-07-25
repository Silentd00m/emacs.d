(defun gears-layers/auto_completion-init ()
  (add-hook 'after-init-hook 'global-company-mode)

  (require 'company)
  (global-company-mode 1)

  (setq company-auto-complete 'ignore
        company-auto-complete-chars nil
        company-backends '(company-bbdb company-capf company-cmake company-keywords company-files company-dabbrev)
        company-continue-commands '(not save-buffer save-some-buffers save-buffers-kill-terminal save-buffers-kill-emacs fixup-whitespace)
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend)
        company-idle-delay 0
        company-minimum-prefix-length 0
        company-selection-wrap-around t
        company-tooltip-align-annotations t)

  (unless gears-show-minor-modes
    (diminish 'company-mode)))

(defun gears-layers/auto_completion-description ()
  "Returns the description of the company layer."

  (eval "Provides autocompletion."))

(defun gears-layers/auto_completion-install ()
  (eval t))

(defun gears-layers/auto_completion-remove ()
  (eval t))

(gears-layer-defdepends auto_completion
                        :packages '(company company-quickhelp))
