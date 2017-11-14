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
  ;; (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "RET") 'c-indent-new-comment-line)
  (yas-minor-mode 1)
  (setq flycheck-clang-args "-std=c++14")
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
  (cond ((gears-check-inside-enum-class (c-langelem-pos langelem)) '-)
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

(defun gears-layers/cpp-install-irony ()
  (irony-install-server (format
                         (concat "%s %s %s && %s --build . "
                                 "--use-stderr --config Release --target install")
                         (shell-quote-argument irony-cmake-executable)
                         (shell-quote-argument (concat "-DCMAKE_INSTALL_PREFIX="
                                                       (expand-file-name
                                                        irony-server-install-prefix)))
                         (shell-quote-argument irony-server-source-dir)
                         (shell-quote-argument irony-cmake-executable))))

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

  "General C++ configuration and package layer.")

(defun gears-layers/cpp-init()
  "Initiaizes the C++ layer."

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Custom options

  (defgroup gears-layers/cpp nil
    "C++ Layer configuration."
    :group 'gears-layers)

  (defcustom gears-layers/cpp-cmake-setup-ide t
    "Setup compiler options and auto-completion."
    :type 'boolean
    :group 'gears-layers/cpp)

  (defcustom gears-layers/cpp-clang-tools-path "/usr/share/clang"
    "Path to clang's emacs tools."
    :type 'string
    :group 'gears-layers/cpp)

  (defcustom gears-layer/cpp-format-and-fix-includes-on-save t
    "Reformat file and fix includes when saving a C++ file."
    :type 'boolean
    :group 'gears-layers/cpp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Prerequisite loading

  (add-to-list 'load-path gears-layers/cpp-clang-tools-path)

  (when (f-exists-p (concat gears-layers/cpp-clang-tools-path
                            "/clang-include-fixer.el"))
    (require 'clang-include-fixer))

  (when (f-exists-p (concat gears-layers/cpp-clang-tools-path
                            "/clang-rename.el"))
    (require 'clang-rename))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Apply options

  (unless gears-show-minor-modes
    (eval-after-load "irony"
      #'(diminish 'irony-mode))

    (eval-after-load "modern-cpp-font-lock"
      #'(diminish 'modern-c++-font-lock-mode))

    (eval-after-load "rtags"
      #'(diminish 'rtags-mode)))

  (setq c-default-style "gears")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Hooks

  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

  (add-hook 'c++-mode-hook 'gears-cpp-mode-hook)

  (add-hook 'before-save-hook #'(lambda ()
                                  (when (or (eq major-mode 'c-mode)
                                            (eq major-mode 'c++-mode))
                                    (when gears-layer/cpp-format-and-fix-includes-on-save
                                      (clang-format-buffer)))))

  (when (and (not (gears-layer-installed-p 'ycmd))
             (gears-layer-installed-p 'auto_completion))
    (add-hook 'company-backends 'company-irony)
    (add-hook 'c++-mode-hook #'(lambda ()
                                 (irony-mode t)

                                 (unless (equal gears-show-documentation-mode 'none)
                                   (irony-eldoc t))

                                 (add-to-list 'company-backends '(company-irony-c-headers
                                                                  company-irony)))))

  (when (gears-layer-installed-p 'dash)
    (add-hook 'c++-mode-hook '(lambda()
                                (message "[Dash] Loaded docset 'C++' and 'C'.")
                                (setq-local helm-dash-docsets '("C++" "C")))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Interface setup

  ;; Add formater to hydra.

  (unless (dynhydra--get-category 'gears-layers/base-hydra-m-f "Format")
    (dynhydra--add-category 'gears-layers/base-hydra-m-f
                            `(,(make-dynhydra-category :title "Format"))))

  (dynhydra-category--add-head (dynhydra--get-category
                                'gears-layers/base-hydra-m-f "Format")
                               `(,(make-dynhydra-head :key "F"
                                                      :text "Format Buffer"
                                                      :command 'clang-format-buffer
                                                      :exit t
                                                      :condition (lambda ()
                                                                   (or (eq major-mode 'c++-mode)
                                                                       (eq major-mode 'c-mode))))))

  (when (not (gears-layer-installed-p 'ycmd))
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

  (when (gears-layer-installed-p 'cmake)
    (gears-layer-init 'cmake)

    (when gears-layers/cpp-cmake-setup-ide
      ;; Also setup rtags if the layer has been installed.
      (add-hook 'c-mode-common-hook #'(lambda ()
                                        (when (gears-layer-installed-p 'rtags)
                                          (require 'rtags))

                                        (when (derived-mode-p 'c-mode 'c++-mode)
                                          ;; (cppcm-reload-all)
                                          (cmake-ide-setup)
                                          (when (gears-layer-installed-p 'rtags)
                                            (cmake-ide-maybe-start-rdm))))))))

(defun gears-layers/cpp-install ()
  "Additional install commands for the C++ layer.")

(gears-layer-defdepends cpp
                        :packages `(modern-cpp-font-lock
                                    ,(unless (gears-layer-installed-p 'ycmd)
                                       'irony)
                                    ,(unless (gears-layer-installed-p 'ycmd)
                                       'irony-eldoc)
                                    ,(when (and (gears-layer-installed-p 'flycheck)
                                                (not (gears-layer-installed-p 'ycmd)))
                                       'flycheck-irony)
                                    ,(when (gears-layer-installed-p 'auto-completion)
                                       'company-irony)
                                    ,(when (gears-layer-installed-p 'auto-completion)
                                       'company-irony-c-headers)
                                    ,(when (gears-layer-installed-p 'cmake)
                                       'cmake-ide)
                                    ,(when (gears-layer-installed-p 'cmake)
                                       'cpputils-cmake)))
