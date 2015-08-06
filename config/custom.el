(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aggressive-indent-comments-too t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(case-fold-search nil)
 '(column-number-mode t)
 '(company-auto-complete (quote ignore))
 '(company-auto-complete-chars nil)
 '(company-backends
   (quote
    (company-c-headers company-ycmd company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf
                       (company-dabbrev-code company-gtags company-etags company-keywords)
                       company-oddmuse company-files company-dabbrev)))
 '(company-continue-commands
   (quote
    (not save-buffer save-some-buffers save-buffers-kill-terminal save-buffers-kill-emacs fixup-whitespace)))
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(company-selection-wrap-around t)
 '(company-ycmd-show-completion-kind t)
 '(custom-enabled-themes (quote (smart-mode-line-respectful)))
 '(custom-safe-themes
   (quote
    ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "87bee8dd980504d41c043e83c24abbbdb780ce5afb9c312f6915ed0aad2630ee" "74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "d7088a7105aa09cc68e3d058f89917e07e0505e0f4ab522a6045ec8092d67c44" "b9530da009f1e1d46bd9fc181b2bcef8c9b1487fedacdad6f306e897b8b8c1f5" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default)))
 '(debug-on-quit nil)
 '(delete-selection-mode t)
 '(dired-find-subdir t)
 '(flycheck-clang-args (quote ("-std=c++11")))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(gdb-many-windows t)
 '(global-company-mode t)
 '(global-flycheck-mode t nil (flycheck))
 '(god-exempt-major-modes helm-mode)
 '(gud-tooltip-mode t)
 '(initial-scratch-message nil)
 '(linum-format "%7d ")
 '(magit-auto-revert-mode nil)
 '(my-global-fci-mode t)
 '(org-CUA-compatible t)
 '(org-replace-disputed-keys nil)
 '(ranger-cleanup-eagerly t)
 '(ranger-key "M-r")
 '(ranger-max-parent-width 0.5)
 '(ranger-max-preview-size 100)
 '(ranger-parent-depth 2)
 '(ranger-preview-file t)
 '(ranger-width-preview 0.5)
 '(recentf-menu-before nil)
 '(recentf-mode t)
 '(save-place t nil (saveplace))
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(smartparens-global-mode t)
 '(smex-prompt-string nil)
 '(ycmd-extra-conf-handler (quote load))
 '(ycmd-keepalive-period 10)
 '(ycmd-parse-conditions (quote (save mode-enabled buffer-focus)))
 '(ycmd-server-args (quote ("--idle_suicide_seconds=10800"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
