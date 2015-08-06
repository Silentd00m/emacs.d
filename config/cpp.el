                                        ;(require 'irony)
                                        ;(require 'irony-cdb)
                                        ;(require 'flycheck)
(require 'ycmd)

(c-add-style "gears"
             '("linux"
               (c-basic-offset . 4)                                             ; 4 spaces
               (c-offsets-alist
                (access-label . -)
                (arglist-intro . +)                                             ;; Indent argument lists
                (arglist-cont . 0)
                (arglist-close . 0)
                (class-close . 0)
                (class-open . 0)
                (defun-block-intro . ++)
                (inclass . +)                                                   ;; Indent classes
                (innamespace . [0])
                (namespace-open . 0)                                            ;; Don't indent namespaces
                (namespace-close . 0)
                (block-close . 0)
                (statement . 0)
                (statement-cont . +)
                (statement-block-intro . +)
                (statement-case-intro . +)                                      ;; Indent case contents
                (topmost-intro-cont . 0)
                (substatement . +)
                (topmost-intro . 0)
                (case-label . +)                                                ;; Indent cases
                (case-label . +)
                (member-init-intro . +)
                (defun-block-intro . +)
                (defun-open . 0)
                (defun-close . 0)
                (defun-block-intro . +)
                (inher-intro . 0)
                (inline-open . 0)
                (member-init-intro . +)
                (member-init-cont . 0)
                (template-args-cont . +)
                (inher-cont . c-lineup-multi-inher)
                )
               )
             )
(setq c-default-style "gears")

(defun gears/cpp-mode-hook ()
  (font-lock-fontify-buffer)
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (yas-minor-mode 1)
  (local-set-key (kbd "C-SPC") 'company-complete)
  (setq flycheck-clang-args "-std=c++11")
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (company-irony-setup-begin-commands)
  (add-to-list 'flycheck-checkers 'irony)
  (fci-mode 1)
  (yas-minor-mode-on)
  (c-set-style "gears")
  (gears/cpp-fix-indent)
  (irony-cdb-autosetup-compile-options)
  (setq flycheck-clang-language-standard "c++11")
  (hideshowvis-minor-mode)
  (irony-eldoc 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language element indentation fix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fixing template indentation
(defun gears-check-inside-template (pos)
  "Checks if POS is a class inside a template."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "template<.*>[\t\n ]*[(class)|(struct)]+")
      )
    )
  )

;; Fixing enum-classes
(defun gears-check-inside-enum-class (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+")
                                        ;(looking-back "enum[ \t]+class[ \t]+")
      )
    )
  )

;; Fix statement-cont
(defun gears-cpp-align-statement-cont (langelem)
  "Fix statement-cont"
  (cond
   ((gears-check-inside-enum-class (c-langelem-pos langelem)) '-)
   ((gears-check-inside-template (c-langelem-pos langelem)) '+)

   ;; Default value
   (t '+)
   )
  )


;; Fix topmost-intro-cont
(defun gears-cpp-align-topmost-intro-cont (langelem)
  "Fix topmost-intro-cont"
  (cond
   ((gears-check-inside-enum-class (c-langelem-pos langelem))
    ;; We're inside a enum-class. Don't indent.
    0)
   )
  )

(defun gears-cpp-align-template-args-cont (langelem)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply fixes

(defun gears-cpp-fix-indent ()
  (interactive)
  (add-to-list 'c-offsets-alist
               '(topmost-intro-cont . gears-cpp-align-topmost-intro-cont)
               )
  (add-to-list 'c-offsets-alist
               '(statement-cont . gears-cpp-align-statement-cont)
               )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YCMD
(set-variable 'ycmd-server-command '("python2" "/home/andre/.emacs.d/ycmd/ycmd"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
;(eval-after-load 'flycheck
;  '(add-to-list 'flycheck-checkers 'flycheck-checker-irony))
(add-to-list 'company-backends 'company-c-headers)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'gears/cpp-mode-hook)
(add-hook 'c-mode-hook 'ycmd-mode)
(add-hook 'c++-mode-hook 'ycmd-mode)
					;(add-hook 'irony-mode-hook 'gears-irony-mode-hook)
