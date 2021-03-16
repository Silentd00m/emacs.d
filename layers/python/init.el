(defgroup gears-layers/python nil
  "Python Layer Configuration"
  :group 'gears-layers)

(defcustom gears-layers/python-show-coverage nil
  "Show coverage test indicators in python-mode."
  :type 'boolean
  :group 'gears-layers/python)

(defcustom gears-layers/python-indent-guide-style 'fill
  "Define how indentation guides should be shown in python-mode.

Options:
 - fill: Fill the background of indentation levels in alternating colors.
 - column: Fill first character of the indentation levels in alternating colors.
 - character: Show a line character to indicate indentation levels.
 - none: Do not show indentation.
"
  :options '('fill 'column 'character 'none)
  :type 'symbolp
  :group 'gears-layers/python)

(use-package highlight-indent-guides
  :ensure t)

(use-package blacken
  :ensure t)

(use-package py-isort
  :ensure t)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init (setq python-indent-guess-indent-offset-verbose nil)
  :config (setq highlight-indent-guides-method
				gears-layers/python-indent-guide-style)
  :hook ((python-mode . blacken-mode)
		 (python-mode . highlight-indent-guides-mode)
		 (rst-mode . (lambda ()
					   (setq indent-tabs-mode nil)))))

(use-package auto-virtualenvwrapper
  :ensure t
  :hook (projectile-after-switch-project-hook . auto-virtualenvwrapper-activate))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred
