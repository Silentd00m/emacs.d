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
                                                     :heads `(,(make-dynhydra-head :key "R"
                                                                                   :text "Show Revisions"
                                                                                   :command '(lambda ()
                                                                                               (interactive)
                                                                                               (git-timemachine)
                                                                                               (dynhydra--open gears-layers/git-hydra-timemachine))
                                                                                   :exit t)
                                                              ,(make-dynhydra-head :key "B"
                                                                                   :text "Blame Mode"
                                                                                   :command 'magit-blame
                                                                                   :exit t)
                                                              ,(make-dynhydra-head :key "L"
                                                                                   :text "Show Log"
                                                                                   :command 'magit-log-all
                                                                                   :exit t)
                                                              ,(make-dynhydra-head :key "S"
                                                                                   :text "Show Status"
                                                                                   :command 'magit-status
                                                                                   :exit t)))))

  (global-git-gutter+-mode)

  (when gears-layers/git-askpass-handler
    (setenv "SSH_ASKPASS" gears-layers/git-askpass-handler))

  (defdynhydra gears-layers/git-hydra-timemachine
    `(,(make-dynhydra-category :title "General"
                               :heads `(,(make-dynhydra-head :key "q"
                                                             :text "Quit Timemachine"
                                                             :command 'git-timemachine-quit
                                                             :exit t)))
    ,(make-dynhydra-category :title "Revisions"
                             :heads `(,(make-dynhydra-head :key "p"
                                                           :text "Previous"
                                                           :command 'git-timemachine-show-previous-revision)
                                      ,(make-dynhydra-head :key "n"
                                                           :text "Next"
                                                           :command 'git-timemachine-show-next-revision)
                                      ,(make-dynhydra-head :key "r"
                                                           :text "Go to Revision"
                                                           :command 'git-timemachine-show-nth-revision))))
    t)

  ;; (dynhydra--set-options (dynhydra--get 'gears-layers/git-hydra-timemachine)
  ;;                        '((foreign-keys run)))
  )

(defun gears-layers/git-description()
  "Returns the description of the git Layer."

  "On the fly syntax checking.")

(defun gears-layers/git-install()
  )

(defun gears-layers/git-remove()
  )

(gears-layer-defdepends git
                        :packages '(magit git-gutter-fringe+ git-timemachine)
                        :layers '(diff))
