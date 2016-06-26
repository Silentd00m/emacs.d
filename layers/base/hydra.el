(load (concat gears-emacs-basepath
              "/functions/hydra"))

;;; Code:

(defvar gears-layers/base-hydra-list '())

(gears-defhydra gears-layers/base-hydra-m-e
                `(,(make-gears-hydra-category :title "Edit"
                                              :heads `(,(make-gears-hydra-head :key "c"
                                                                               :text "Copy Selection"
                                                                               :command 'cua-copy-region)
                                                       ,(make-gears-hydra-head :key "x"
                                                                               :text "Cut Selection"
                                                                               :command 'cua-cut-region)
                                                       ,(make-gears-hydra-head :key "v"
                                                                               :text "Paste Clipboard"
                                                                               :command 'cua-paste)
                                                       ,(make-gears-hydra-head :key "u"
                                                                               :text "Undo"
                                                                               :command 'undo-tree-undo)
                                                       ,(make-gears-hydra-head :key "r"
                                                                               :text "Redo"
                                                                               :command 'undo-tree-redo)
                                                       ,(make-gears-hydra-head :key "h"
                                                                               :text "Show History"
                                                                               :command 'undo-tree-visualize)))
                  ,(make-gears-hydra-category :title "Eval/Execute"
                                              :heads `(,(make-gears-hydra-head :key "R"
                                                                               :text "Selected Region"
                                                                               :command 'eval-region
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "b"
                                                                               :text "Whole Buffer"
                                                                               :command 'eval-buffer
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "f"
                                                                               :text "Current Function"
                                                                               :command 'eval-defun
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "e"
                                                                               :text "Expression"
                                                                               :command 'eval-expression
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "C"
                                                                               :text "Command"
                                                                               :command 'helm-M-x
                                                                               :exit t)))))

(gears-defhydra gears-layers/base-hydra-m-f
                `(,(make-gears-hydra-category :title "File"
                                              :heads `(,(make-gears-hydra-head :key "s"
                                                                               :text "Save"
                                                                               :command 'save-buffer
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "o"
                                                                               :text "Open"
                                                                               :command 'helm-find-files
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "n"
                                                                               :text "New"
                                                                               :command 'find-file
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "b"
                                                                               :text "Filebrowser"
                                                                               :command 'ranger
                                                                               :exit t)))))

(gears-defhydra gears-layers/base-hydra-m-g
                `(,(make-gears-hydra-category :title "GOTO"
                                              :heads `(,(make-gears-hydra-head :key "l"
                                                                               :text "Go to Line"
                                                                               :command 'goto-line
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "c"
                                                                               :text "Go to Character"
                                                                               :command 'goto-char
                                                                               :exit t)))
                  ,(make-gears-hydra-category :title "GIT"
                                              :heads `(,(make-gears-hydra-head :key "s"
                                                                               :text "Show Status"
                                                                               :command 'magit-status
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "b"
                                                                               :text "Blame Mode"
                                                                               :command 'magit-blame
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "L"
                                                                               :text "Show Log"
                                                                               :command 'magit-log-all
                                                                               :exit t)))))

(gears-defhydra gears-layers/base-hydra-m-p
                `(,(make-gears-hydra-category :title "Project"
                                              :heads `(,(make-gears-hydra-head :key "p"
                                                                               :text "Open Project Buffers and Files"
                                                                               :command 'helm-projectile
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "s"
                                                                               :text "Switch Project"
                                                                               :command 'helm-projectile-switch-project
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "f"
                                                                               :text "Find File in Project"
                                                                               :command 'helm-projectile-find-dwim
                                                                               :exit t)))))


(gears-defhydra gears-layers/base-hydra-m-r
                `(,(make-gears-hydra-category :title "Buffer"
                                              :heads `(,(make-gears-hydra-head :key "R"
                                                                               :text "Reload File"
                                                                               :command 'revert-buffer
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "y"
                                                                               :text "Redo"
                                                                               :command 'undo-tree-redo)))))

(gears-defhydra gears-layers/base-hydra-m-s
                `(,(make-gears-hydra-category :title "Buffer"
                                              :heads `(,(make-gears-hydra-head :key "s"
                                                                               :text "Save"
                                                                               :command 'save-buffer
                                                                               :exit t)
                                                       ,(make-gears-hydra-head :key "a"
                                                                               :text "Select Everything"
                                                                               :command 'mark-whole-buffer)))
                  ,(make-gears-hydra-category :title "Split"
                                              :heads `(,(make-gears-hydra-head :key "h"
                                                                               :text "Horizontal"
                                                                               :command 'split-window-horizontally)
                                                       ,(make-gears-hydra-head :key "v"
                                                                               :text "Vertical"
                                                                               :command 'split-window-vertically)
                                                       ,(make-gears-hydra-head :key "c"
                                                                               :text "Close Current"
                                                                               :command 'delete-window)
                                                       ,(make-gears-hydra-head :key "o"
                                                                               :text "Close Other"
                                                                               :command 'delete-other-windows)))
                  ,(make-gears-hydra-category :title "Search"
                                              :heads `(,(make-gears-hydra-head :key "b"
                                                                               :text "In Buffer"
                                                                               :command 'helm-occur
                                                                               :exit t)
                                                       ;; ,(make-gears-hydra-head :key "f"
                                                       ;;                         :text "In Filesystem"
                                                       ;;                         :command 'helm-grep)
                                                       ))))

(gears-defhydra gears-layers/base-hydra-m-t
                `(,(make-gears-hydra-category :title "Toggle"
                                              :heads `(,(make-gears-hydra-head :key "c"
                                                                               :text "Comment"
                                                                               :command 'comment-dwim-2)
                                                       ,(make-gears-hydra-head :key "r"
                                                                               :text "Region"
                                                                               :command 'origami-forward-toggle-node
                                                                               :exit t)))))
