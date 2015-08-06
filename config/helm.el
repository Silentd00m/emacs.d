(require 'helm)
(require 'helm-config)

(helm-mode 1)
(setq helm-quick-update t)
(setq helm-X-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-matching t)
(setq helm-move-to-line-cycle-in-source t)
(setq helm-candidate-number-limit 30)
(helm-autoresize-mode 1)
(helm-occur-init-source)
(helm-attrset 'follow 1 helm-source-occur)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "ESC") 'helm-keyboard-quit)

(add-hook 'after-init-hook '(lambda()
                              (find-file helm-find-files nil remap) ; Replace find-file with helm-find-files
                              (execute-extended-command helm-M-x nil remap) ; Replace execute-command with helm-M-x
                              )
          )
;(add-hook 'helm-mode-hook '(fci-mode -1))

(defun gears/helm-find-files-navigate-back (orig-fun &rest args)
  "Go up one directory when pressing backspace in a directory when no additional"
  "Characters have been entered."
  (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
      (helm-find-files-up-one-level 1)
    (apply orig-fun args)))
(advice-add 'helm-ff-delete-char-backward :around #'gears/helm-find-files-navigate-back)
