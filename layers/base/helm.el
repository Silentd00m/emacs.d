(require 'helm-config)

(helm-mode 1)
(setq helm-quick-update t)
(setq helm-X-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-matching t)
(setq helm-move-to-line-cycle-in-source t)
(setq helm-semantic-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-imenu-fuzzy-match t)
(setq helm-candidate-number-limit 30)
(helm-autoresize-mode 1)
(helm-occur-init-source)
(helm-attrset 'follow 1 helm-source-occur)

(defcustom gears-helm-enable-tab-completion t
  "Enables completion using the <tab> key when inside a helm buffer."
  :type 'boolean
  :group 'gears-interface)

(defcustom gears-helm-enable-close-on-esc t
  "Enables closing helm using the <ESC> key."
  :type 'boolean
  :group 'gears-interface)

(when gears-helm-enable-tab-completion
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action))

(when gears-helm-enable-close-on-esc
  (define-key helm-map (kbd "ESC") 'helm-keyboard-quit))

(helm-flx-mode 1)

(require 'helm-projectile)

(defun gears-helm-mini ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)

  (require 'helm-for-files)
  ;; (require 'helm-files)
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
(advice-add 'helm-ff-delete-char-backward :around #'gears/helm-find-files-navigate-back)

(defun gears/helm-find-files-navigate-back (orig-fun &rest args)
  "Go up one directory when pressing backspace in a directory when no additional
  Characters have been entered."
  (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
      (helm-find-files-up-one-level 1)
    (apply orig-fun args)))
