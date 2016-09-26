(defun gears-layers/git-init()
  (require 'git-gutter-fringe+)

  (unless gears-show-minor-modes
    (diminish 'git-gutter+-mode))

  (dynhydra--add-category 'gears-layers/base-hydra-m-g
                          `(,(make-dynhydra-category :title "GIT"
                                                     :heads `(,(make-dynhydra-head :key "S"
                                                                                   :text "Show Status"
                                                                                   :command 'magit-status
                                                                                   :exit t)
                                                              ,(make-dynhydra-head :key "B"
                                                                                   :text "Blame Mode"
                                                                                   :command 'magit-blame
                                                                                   :exit t)
                                                              ,(make-dynhydra-head :key "L"
                                                                                   :text "Show Log"
                                                                                   :command 'magit-log-all
                                                                                   :exit t)))))

  (global-git-gutter+-mode))

(defun gears-layers/git-description()
  "Returns the description of the git Layer."

  "On the fly syntax checking.")

(defun gears-layers/git-install()
  )

(defun gears-layers/git-remove()
  )

(gears-layer-defdepends git
                        :packages '(magit git-gutter-fringe+))
