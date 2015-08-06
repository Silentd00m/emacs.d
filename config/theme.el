;(require 'darcula-theme)
                                        ;(require 'indent-guide)
(require 'rainbow-delimiters)
(require 'nlinum)
                                        ;(require 'sr-speedbar)
(require 'moe-theme)
(load-theme 'moe-dark t)
(tool-bar-mode -1)                                                              ; Disable toolbar

(set-default-font "SourceCodePro-10")
(set-frame-font "SourceCodePro-10")
(global-nlinum-mode t)
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
(global-hl-line-mode 1)                                                         ; Turn on highlighting current line
                                        ;(indent-guide-global-mode)                                                      ; Show indent guides
(show-paren-mode 1)                                                             ; Show matching delimiters.
(setq show-paren-style 'expression)
(column-number-mode t)

;; Maximize window on start
                                        ;(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq font-lock-maximum-decoration 6)


;; Fill column indicator
(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(setq-default fci-rule-color "firebrick")
(setq whitespace-style '(face lines-tail))
(defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))
(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;(global-fci-mode 1)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (setq sanityinc/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

;; Smart Mode Line
(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'dark)
(setq sml/no-confirm-load-theme t)

;; Whitespace
(require 'whitespace)

(setq whitespace-line-column 80) ;; limit line length
(add-hook 'prog-mode-hook 'whitespace-mode)
