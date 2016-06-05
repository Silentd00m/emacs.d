(use-package flycheck
  :ensure t
  :config (progn (setq flycheck-highlighting-mode `symbols)
                 (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package flycheck-ycmd
  :ensure flycheck
  :config (progn (flycheck-ycmd-setup)
                 (flycheck-pos-tip-mode)
                 (eval-after-load 'flycheck
                   '(custom-set-variables
                     '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))))
