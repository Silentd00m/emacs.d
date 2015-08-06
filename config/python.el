;(require 'anaconda-mode)
					;(autoload 'jedi:setup "jedi" nil t)

					;(setq jedi:complete-on-dot t)
					;(add-hook 'python-mode-hook 'jedi:setup)
					;(setq jedi:setup-keys t)
					;(add-hook 'python-mode-hook 'jedi:ac-setup)
                                        ;(setq jedi:server-command '("~/.emacs.d/plugins/emacs-jedi/jediepcserver.py"))
					;(setq jedi:server-command '("/usr/bin/jediepcserver"))
(add-hook 'python-mode-hook '(lambda ()
                               (ycmd-mode)
                               (eldoc-mode)
                               (hideshowvis-minor-mode)
                               (fci-mode t)
                               )
          )

(add-to-list 'company-backends 'company-ycmd)
(add-hook 'python-mode-hook 'ycmd-mode)

(defun my/python-first-run-hook ()
  (remove-hook 'python-mode-hook 'my/python-first-run-hook)
  (run-python))

(add-hook 'python-mode-hook 'my/python-first-run-hook)

					;(add-to-list 'company-backends 'company-anaconda)
