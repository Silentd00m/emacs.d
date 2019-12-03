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

(use-package pycoverage
  :ensure t
  :hook (python-mode-hook . pycoverage-mode))

(use-package highlight-indent-guides
  :ensure t)

(use-package py-yapf
  :ensure t)

(use-package python
  :after lsp
  :config (setq highlight-indent-guides-method
                gears-layers/python-indent-guide-style)
  :hook ((python-mode-hook . highlight-indent-guides-mode)
         (python-mode-hook . lsp)))
