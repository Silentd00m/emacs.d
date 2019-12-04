(defgroup gears-layers/git nil
  "Git Layer Configuration"

  :group 'gears-layers)

(defcustom gears-layers/git-askpass-handler nil
  "Askpass handler. No handler if empty."

  :type '(file :must-match t)
  :set #'(lambda (sym val)
           (custom-set-default sym val)
           (setenv "SSH_ASKPASS" gears-layers/git-askpass-handler)))

(use-package magit
  :ensure t
  :init (progn (dynhydra-add 'M-g
                             '(("S" magit-status "Status" :column "Git" :exit t)
                               ("B" magit-blame "Blame" :column "Git" :exit t)
                               ("L" magit-log-all "Log" :column "Git" :exit t)
                               ("R" git-timemachine "Revisions" :column "Git" :exit t)))

               (with-eval-after-load 'magit
                 (dolist (face '(magit-diff-hunk-heading
                                 magit-diff-hunk-heading-highlight
                                 magit-diff-hunk-heading-selection
                                 magit-diff-hunk-region
                                 magit-diff-lines-heading
                                 magit-diff-lines-boundary
                                 magit-diff-conflict-heading
                                 magit-diff-added
                                 magit-diff-removed
                                 magit-diff-our
                                 magit-diff-base
                                 magit-diff-their
                                 magit-diff-context
                                 magit-diff-added-highlight
                                 magit-diff-removed-highlight
                                 magit-diff-our-highlight
                                 magit-diff-base-highlight
                                 magit-diff-their-highlight
                                 magit-diff-context-highlight
                                 magit-diff-whitespace-warning
                                 magit-diffstat-added
                                 magit-diffstat-removed
                                 magit-section-heading
                                 magit-section-heading-selection
                                 magit-section-highlight
                                 magit-section-secondary-heading
                                 magit-diff-file-heading
                                 magit-diff-file-heading-highlight
                                 magit-diff-file-heading-selection))
                   (when (>= emacs-major-version 27)
                     (set-face-attribute face nil :extend t))))))

(use-package diff-hl
  :ensure t
  :config (progn (global-diff-hl-mode)
                 (diminish 'diff-hl-mode)))

(use-package git-timemachine
  :ensure t
  :hydra (git-timemachine-hydra (:color pink :hint nil)
                                ("p" git-timemachine-show-previous-revision "Previous" :column "Revisions")
                                ("n" git-timemachine-show-next-revision "Next" :column "Revisions")
                                ("g" git-timemachine-show-nth-revision "GoTo Revision" :column "Revisions")
                                ("q" git-timemachine-quit "Quit" :color blue :exit t))
  :hook (git-timemachine-mode . git-timemachine-hydra/body))

(use-package vdiff
  :ensure t
  :init (setq vdiff-lock-scrolling t))

;; (use-package vdiff-magit
;;   :ensure t
;;   :bind (:map magit-mode-map
;;               ("e" . vdiff-magit-dwim)
;;               ("E" . vdiff-magit))
;;   :after (vdiff magit)
;;   :config (progn)
;;   :init (progn (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
;;                (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
;;                (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
;;                (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)))
