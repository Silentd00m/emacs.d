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
                (comment-intro . 0))))

(defun gears-cpp-mode-hook ()
  (setq font-lock-maximum-decoration 6)
  (font-lock-fontify-buffer)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (yas-minor-mode 1)
  (setq flycheck-clang-args "-std=c++11")
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;; (fci-mode 1)
  (yas-minor-mode-on)
  (c-set-style "gears")
  (gears-cpp-fix-indent)
  (setq flycheck-clang-language-standard "c++11")
  (hideshowvis-minor-mode))

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
      (looking-back "template<.*>[\t\n ]*[(class)|(struct)]+"))))

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
      )))

;; Fix statement-cont
(defun gears-cpp-align-statement-cont (langelem)
  "Fix statement-cont"
  (cond
   ((gears-check-inside-enum-class (c-langelem-pos langelem)) '-)
   ((gears-check-inside-template (c-langelem-pos langelem)) '+)

   ;; Default value
   (t '+)))


;; Fix topmost-intro-cont
(defun gears-cpp-align-topmost-intro-cont (langelem)
  "Fix topmost-intro-cont"
  (cond ((gears-check-inside-enum-class (c-langelem-pos langelem))
         ;; We're inside a enum-class. Don't indent.
         0)))

(defun gears-cpp-align-template-args-cont (langelem)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply fixes

(defun gears-cpp-fix-indent ()
  (interactive)
  (add-to-list 'c-offsets-alist
               '(topmost-intro-cont . gears-cpp-align-topmost-intro-cont))
  (add-to-list 'c-offsets-alist
               '(statement-cont . gears-cpp-align-statement-cont)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gears-layers/cpp-description()
  "Returns the description of the C++ layer."

  (eval "General C++ configuration and package layer."))

(defun gears-layers/cpp-init()
  "Initiaizes the C++ layer."

  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

  (setq c-default-style "gears")

  (when (boundp 'company-backends)
    (add-to-list 'company-backends 'company-c-headers))
  (add-hook 'c++-mode-hook 'gears-cpp-mode-hook)
  (when (gears-layer-installed "dash")
    (add-hook 'c++-mode-hook '(lambda()
                                (message "[Dash] Loaded docset 'C++' and 'C'.")
                                (setq-local helm-dash-docsets '("C++" "C"))))))

(defun gears-layers/cpp-install()
  "Additional install commands for the C++ layer.")

(gears-layer-defdepends cpp
                        :packages '(modern-cpp-font-lock))
