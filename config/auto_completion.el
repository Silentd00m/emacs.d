(require 'fuzzy)
(require 'yasnippet)
(require 'smartparens-config)
(require 'company)

(add-hook 'after-init-hook 'global-company-mode)

(set-default 'semantic-case-fold t)

;; Command completion
;(smex-initialize)

;; Parenthese auto-completion
(smartparens-global-mode)

;; Helm
(load "~/.emacs.d/config/helm.el")

;; YCMD
(require 'company-ycmd)
(company-ycmd-setup)

;; company-mode

(company-quickhelp-mode 1)
