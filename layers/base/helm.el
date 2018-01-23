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

;; https://gist.github.com/fuxialexander/5ad46671689d96a29f9865c1c0b42d10
(defun gears-helm-display-child-frame (buffer)
    "Display `helm-buffer' in a separate frame.
Function suitable for `helm-display-function',
`helm-completion-in-region-display-function'
and/or `helm-show-completion-default-display-function'.
See `helm-display-buffer-height' and `helm-display-buffer-width' to
configure frame size."
    (if (not (display-graphic-p))
        ;; Fallback to default when frames are not usable.
        (helm-default-display-buffer buffer)
      (setq helm--buffer-in-new-frame-p t)
      (let* ((pos (window-absolute-pixel-position))
             (half-screen-size (/ (display-pixel-height x-display-name) 2))
             (frame-info (frame-geometry))
             (prmt-size (length helm--prompt))
             (line-height (frame-char-height))
             (default-frame-alist
               `((parent . ,(selected-frame))
                 (width . ,helm-display-buffer-width)
                 (height . ,helm-display-buffer-height)
                 (undecorated . t)
                 (left-fringe . 0)
                 (right-fringe . 0)
                 (tool-bar-lines . 0)
                 (line-spacing . 0)
                 (desktop-dont-save . t)
                 (no-special-glyphs . t)
                 (inhibit-double-buffering . t)
                 (tool-bar-lines . 0)
                 (left . ,(- (car pos)
                             (* (frame-char-width)
                                (if (< (- (point) (point-at-bol)) prmt-size)
                                    (- (point) (point-at-bol))
                                  prmt-size))))
                 ;; Try to put frame at the best possible place.
                 ;; Frame should be below point if enough
                 ;; place, otherwise above point and
                 ;; current line should not be hidden
                 ;; by helm frame.
                 (top . ,(if (> (cdr pos) half-screen-size)
                             ;; Above point
                             (- (cdr pos)
                                ;; add 2 lines to make sure there is always a gap
                                (* (+ helm-display-buffer-height 2) line-height)
                                ;; account for title bar height too
                                (cddr (assq 'title-bar-size frame-info)))
                           ;; Below point
                           (+ (cdr pos) line-height)))
                 (title . "Helm")
                 (vertical-scroll-bars . nil)
                 (menu-bar-lines . 0)
                 (fullscreen . nil)
                 (visible . ,(null helm-display-buffer-reuse-frame))
                 (minibuffer . t)))
             display-buffer-alist)
        ;; Add the hook inconditionally, if
        ;; helm-echo-input-in-header-line is nil helm-hide-minibuffer-maybe
        ;; will have anyway no effect so no need to remove the hook.
        (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
        (with-helm-buffer
          (setq-local helm-echo-input-in-header-line
                      (not (> (cdr pos) half-screen-size))))
        (helm-display-buffer-popup-frame buffer default-frame-alist))
      (helm-log-run-hook 'helm-window-configuration-hook)))

;; (setq helm-display-function 'helm-display-buffer-in-own-frame
;;       helm-display-buffer-reuse-frame t
;;       helm-use-undecorated-frame-option t)

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
