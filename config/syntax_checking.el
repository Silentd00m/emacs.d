(use-package flycheck
  :ensure t
  :config (progn (setq flycheck-highlighting-mode `symbols)
                 (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package flycheck-ycmd
  :ensure flycheck
  :config (progn (flycheck-ycmd-setup)
                 (flycheck-pos-tip-mode)
                 (eval-after-load 'flycheck
                   '(when (not (display-graphic-p))
                      (setq flycheck-indication-mode nil)))))
