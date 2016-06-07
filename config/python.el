(use-package mmm-mode
  :ensure t
  :config (progn (require 'mmm-auto)
                 (setq mmm-global-mode 'maybe)))

(add-hook 'python-mode-hook '(lambda ()
                               (ycmd-mode)
                               (eldoc-mode)
                               (hideshowvis-minor-mode)
                               (fci-mode t)))

(defun my/python-first-run-hook()
  (remove-hook 'python-mode-hook 'my/python-first-run-hook)
  (run-python))

(add-hook 'python-mode-hook 'my/python-first-run-hook)
(add-hook 'python-mode-hook '(lambda()
                               (message "[Dash] Loaded docset 'Python 3'")
                               (helm-dash-activate-docset "Python 3")))

                                        ;(add-to-list 'company-backends 'company-anaconda)

;; Mako

(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . python-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)
