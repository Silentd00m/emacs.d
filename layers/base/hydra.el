(load (concat gears-emacs-basepath
              "/functions/hydra"))

;;; Code:

;; (defvar gears-layers/base-hydra-list '())

(defdynhydra gears-layers/base-hydra-m-a
  `(,(make-dynhydra-category :title "Align"
                             :heads `(,(make-dynhydra-head :key "s"
                                                           :text "Selection"
                                                           :command 'align-current
                                                           :exit t)
                                      ,(make-dynhydra-head :key "f"
                                                           :text "File"
                                                           :command 'align-entire
                                                           :exit t)))))

(defdynhydra gears-layers/base-hydra-m-b `())

(defdynhydra gears-layers/base-hydra-m-c
  `(,(make-dynhydra-category :title "Selection"
                             :heads `(,(make-dynhydra-head :key "c"
                                                           :text "Copy"
                                                           :command 'cua-copy-region)
                                      ,(make-dynhydra-head :key "x"
                                                           :text "Cut"
                                                           :command 'cua-cut-region)))
    ,(make-dynhydra-category :title "Convert"
                             :heads `(,(make-dynhydra-head :key "L"
                                                           :text "Lowercase"
                                                           :command 'downcase-dwim)
                                      ,(make-dynhydra-head :key "U"
                                                           :text "Uppercase"
                                                           :command 'upcase-dwim)
                                      ,(make-dynhydra-head :key "C"
                                                           :text "Capitalize"
                                                           :command 'capitalize-dwim)))))

(defdynhydra gears-layers/base-hydra-m-d `())

(defdynhydra gears-layers/base-hydra-m-e
  `(,(make-dynhydra-category :title "Edit"
                             :heads `(,(make-dynhydra-head :key "c"
                                                           :text "Copy Selection"
                                                           :command 'cua-copy-region)
                                      ,(make-dynhydra-head :key "x"
                                                           :text "Cut Selection"
                                                           :command 'cua-cut-region
                                                           :exit t)
                                      ,(make-dynhydra-head :key "v"
                                                           :text "Paste Clipboard"
                                                           :command 'cua-paste)
                                      ,(make-dynhydra-head :key "u"
                                                           :text "Undo"
                                                           :command 'undo-tree-undo)
                                      ,(make-dynhydra-head :key "r"
                                                           :text "Redo"
                                                           :command 'undo-tree-redo)
                                      ,(make-dynhydra-head :key "h"
                                                           :text "Show History"
                                                           :command 'undo-tree-visualize
                                                           :exit t)))
    ,(make-dynhydra-category :title "Eval/Execute"
                             :heads `(,(make-dynhydra-head :key "R"
                                                           :text "Selected Region"
                                                           :command 'eval-region
                                                           :exit t)
                                      ,(make-dynhydra-head :key "b"
                                                           :text "Whole Buffer"
                                                           :command 'eval-buffer
                                                           :exit t)
                                      ,(make-dynhydra-head :key "f"
                                                           :text "Current Function"
                                                           :command 'eval-defun
                                                           :exit t)
                                      ,(make-dynhydra-head :key "e"
                                                           :text "Expression"
                                                           :command 'eval-expression
                                                           :exit t)
                                      ,(make-dynhydra-head :key "C"
                                                           :text "Command"
                                                           :command 'helm-M-x
                                                           :exit t)))))

(defdynhydra gears-layers/base-hydra-m-f
  `(,(make-dynhydra-category :title "File"
                             :heads `(,(make-dynhydra-head :key "s"
                                                           :text "Save"
                                                           :command 'save-buffer
                                                           :exit t)
                                      ,(make-dynhydra-head :key "o"
                                                           :text "Open"
                                                           :command 'helm-find-files
                                                           :exit t)
                                      ,(make-dynhydra-head :key "n"
                                                           :text "New"
                                                           :command 'find-file
                                                           :exit t)
                                      ,(make-dynhydra-head :key "b"
                                                           :text "Filebrowser"
                                                           :command 'ranger
                                                           :exit t)))))

(defdynhydra gears-layers/base-hydra-m-g
  `(,(make-dynhydra-category :title "GOTO"
                             :heads `(,(make-dynhydra-head :key "l"
                                                           :text "Go to Line"
                                                           :command 'goto-line
                                                           :exit t)
                                      ,(make-dynhydra-head :key "c"
                                                           :text "Go to Character"
                                                           :command 'goto-char
                                                           :exit t)))))

(defdynhydra gears-layers/base-hydra-m-p
  `(,(make-dynhydra-category :title "Project"
                             :heads `(,(make-dynhydra-head :key "p"
                                                           :text "Open Project Buffers and Files"
                                                           :command 'helm-projectile
                                                           :exit t)
                                      ,(make-dynhydra-head :key "s"
                                                           :text "Switch Project"
                                                           :command 'helm-projectile-switch-project
                                                           :exit t)
                                      ,(make-dynhydra-head :key "f"
                                                           :text "Find File in Project"
                                                           :command 'helm-projectile-find-dwim
                                                           :exit t)))))


(defdynhydra gears-layers/base-hydra-m-r
  `(,(make-dynhydra-category :title "Buffer"
                             :heads `(,(make-dynhydra-head :key "R"
                                                           :text "Reload File"
                                                           :command 'revert-buffer
                                                           :exit t)
                                      ,(make-dynhydra-head :key "y"
                                                           :text "Redo"
                                                           :command 'undo-tree-redo)
                                      ,(make-dynhydra-head :key "r"
                                                           :text "Replace"
                                                           :command 'vr/replace)))
    ,(when gears-enable-semantic-mode
       (make-dynhydra-category :title "Refactor"
                                :heads `(,(make-dynhydra-head :key "M"
                                                              :text "Show Menu"
                                                              :command 'srefactor-refactor-at-point
                                                              :exit t
                                                              :condition (lambda ()
                                                                           (and gears-enable-semantic-mode
                                                                                (or (eq major-mode 'c++-mode)
                                                                                    (eq major-mode 'c-mode)
                                                                                    (eq major-mode 'python-mode)
                                                                                    (eq major-mode 'lisp-mode)
                                                                                    (eq major-mode 'emacs-lisp-mode))))))))))

(defdynhydra gears-layers/base-hydra-m-s
  `(,(make-dynhydra-category :title "Buffer"
                             :heads `(,(make-dynhydra-head :key "s"
                                                           :text "Save"
                                                           :command 'save-buffer
                                                           :exit t)
                                      ,(make-dynhydra-head :key "a"
                                                           :text "Select Everything"
                                                           :command 'mark-whole-buffer
                                                           :exit t)))
    ,(make-dynhydra-category :title "Split"
                             :heads `(,(make-dynhydra-head :key "h"
                                                           :text "Horizontal"
                                                           :command 'split-window-horizontally
                                                           :exit t)
                                      ,(make-dynhydra-head :key "v"
                                                           :text "Vertical"
                                                           :command 'split-window-vertically
                                                           :exit t)
                                      ,(make-dynhydra-head :key "c"
                                                           :text "Close Current"
                                                           :command 'delete-window
                                                           :exit t)
                                      ,(make-dynhydra-head :key "o"
                                                           :text "Close Other"
                                                           :command 'delete-other-windows
                                                           :exit t)))
    ,(make-dynhydra-category :title "Search"
                             :heads `(,(make-dynhydra-head :key "b"
                                                           :text "In Buffer"
                                                           :command 'helm-occur
                                                           :exit t)))))

(defdynhydra gears-layers/base-hydra-m-t
  `(,(make-dynhydra-category :title "Toggle"
                             :heads `(,(make-dynhydra-head :key "c"
                                                           :text "Comment"
                                                           :command 'comment-dwim-2)
                                      ,(make-dynhydra-head :key "r"
                                                           :text "Region"
                                                           :command 'origami-forward-toggle-node
                                                           :exit t)))))
