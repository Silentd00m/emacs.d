(defgroup gears-layers/git nil
  "Git Layer Configuration"

  :group 'gears-layers)

(defcustom gears-layers/git-askpass-handler nil
  "Askpass handler. No handler if empty."

  :type 'file)

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

  (global-git-gutter+-mode)

  (when gears-layers/git-askpass-handler
    (setenv "SSH_ASKPASS" gears-layers/git-askpass-handler)))

(defun gears-layers/git-description()
  "Returns the description of the git Layer."

  "On the fly syntax checking.")

(defun gears-layers/git-install()
  )

(defun gears-layers/git-remove()
  )

(gears-layer-defdepends git
                        :packages '(magit git-gutter-fringe+))
