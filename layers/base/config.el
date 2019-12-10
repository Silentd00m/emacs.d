;;; Code:
;; Whitespace

;; Indentation

;; General

;; Theme

(defun gears-layers/base/config-font (val)
  "Setter callback function for gears-font."

  (set-frame-font val)
  (set-face-attribute 'default nil :font val)
  ;(set-default-font val)
  )

;; Interface

(defun gears-layers/base/config-show-current-context (val)
  "Setter callback function for gears-show-current-context."

  (when gears-enable-semantic-mode
    (if val
        (add-to-list 'semantic-inhibit-functions
                     (lambda ()
                       (member major-mode '(html-mode
                                            lisp-mode
                                            emacs-lisp-mode))))
      (setq semantic-inhibit-functions
            (remove (lambda ()
                      (member major-mode '(html-mode
                                           lisp-mode
                                           emacs-lisp-mode)))
                    semantic-inhibit-functions)))))

;; Keyboard

(defun gears-layers/base/config-global-keymap (val)
  "Setter callback function for gears-autoresize-splits."

  (dolist (i val)
    (global-set-key (kbd (car i)) (cdr i))))
