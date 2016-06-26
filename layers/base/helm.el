(use-package helm
  :ensure t
  :config (progn (require 'helm-config)

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

                 (require 'helm-projectile)

                 (defun gears-helm-mini ()
                   "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
                   (interactive)
                   (require 'helm-files)
                   (unless helm-source-buffers-list
                     (setq helm-source-buffers-list
                           (helm-make-source "Buffers" 'helm-source-buffers)))
                   (let ((ghm-source-list '(helm-source-buffers-list
                                            helm-source-projectile-files-list
                                            helm-source-recentf
                                            helm-source-buffer-not-found
                                            helm-source-projectile-projects)))

                     (helm :sources ghm-source-list
                           :buffer "*helm mini*"
                           :ff-transformer-show-only-basename nil
                           :truncate-lines helm-buffers-truncate-lines)))

                 ;; (require 'helm-fzf)
                 (advice-add 'helm-ff-delete-char-backward :around #'gears/helm-find-files-navigate-back)))

(use-package fzf
  :ensure t)

(use-package helm-flycheck
  :ensure t)

(defun gears/helm-find-files-navigate-back (orig-fun &rest args)
  "Go up one directory when pressing backspace in a directory when no additional"
  "Characters have been entered."
  (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
      (helm-find-files-up-one-level 1)
    (apply orig-fun args)))