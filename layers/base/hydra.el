(load (concat gears-emacs-basepath
              "/functions/hydra"))

;;; Code:

(defvar gears-layers/base-hydra-list '())

(gears-defhydra gears-layers/base-hydra-m-p
                `(,(make-gears-hydra-category :title "Project"
                                              :heads `(,(make-gears-hydra-head :key "p"
                                                                               :text "Open Project Buffers and Files"
                                                                               :command 'helm-projectile)
                                                       ,(make-gears-hydra-head :key "s"
                                                                               :text "Switch Project"
                                                                               :command 'helm-projectile-switch-project)
                                                       ,(make-gears-hydra-head :key "f"
                                                                               :text "Find File in Project"
                                                                               :command 'helm-projectile-find-dwim)))))

(gears-defhydra gears-layers/base-hydra-m-g
                `(,(make-gears-hydra-category :title "GOTO"
                                              :heads `(,(make-gears-hydra-head :key "l"
                                                                               :text "Go to Line"
                                                                               :command 'goto-line)
                                                       ,(make-gears-hydra-head :key "c"
                                                                               :text "Go to Character"
                                                                               :command 'goto-char)))
                  ,(make-gears-hydra-category :title "GIT"
                                              :heads `(,(make-gears-hydra-head :key "s"
                                                                               :text "Show Status"
                                                                               :command 'magit-status)
                                                       ,(make-gears-hydra-head :key "b"
                                                                               :text "Blame Mode"
                                                                               :command 'magit-blame)
                                                       ,(make-gears-hydra-head :key "L"
                                                                               :text "Show Log"
                                                                               :command 'magit-log-all)))))
