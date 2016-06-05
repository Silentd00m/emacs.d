(use-package org
  :ensure t
  :config (progn (setq org-todo-keyword-faces '(("TODO" . "red")
                                                ("NEXT" . "blue")
                                                ("MAYBE" . "purple")
                                                ("DONE" . "green")
                                                ("CANCELLED" . "orange")))))
