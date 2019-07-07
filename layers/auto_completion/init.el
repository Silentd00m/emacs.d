(defun gears-layers/auto_completion-init ()
  (add-hook 'after-init-hook 'global-company-mode)

  (require 'company)
  (require 'company-box)

  (global-company-mode 1)

  (setq company-auto-complete 'ignore
        company-auto-complete-chars nil
        company-backends '(company-bbdb company-capf company-cmake company-keywords company-files company-dabbrev)
        company-continue-commands '(not save-buffer save-some-buffers save-buffers-kill-terminal save-buffers-kill-emacs fixup-whitespace)
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend)
        company-idle-delay 0
        company-minimum-prefix-length 0
        company-selection-wrap-around t
        company-tooltip-align-annotations t)

  (add-hook 'company-mode-hook 'company-box-mode)

  (setq company-box-icons-unknown 'fa_question_circle)

  (setq company-box-icons-elisp
        '((fa_tag :face font-lock-function-name-face) ;; Function
          (fa_cog :face font-lock-variable-name-face) ;; Variable
          (fa_cube :face font-lock-constant-face) ;; Feature
          (md_color_lens :face font-lock-doc-face))) ;; Face

  (setq company-box-icons-yasnippet 'fa_bookmark)

  (setq company-box-icons-lsp
        '((1 . fa_text_height) ;; Text
          (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
          (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
          (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
          (5 . (fa_cog :foreground "#FF9800")) ;; Field
          (6 . (fa_cog :foreground "#FF9800")) ;; Variable
          (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
          (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
          (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
          (10 . (fa_cog :foreground "#FF9800")) ;; Property
          (11 . md_settings_system_daydream) ;; Unit
          (12 . (fa_cog :foreground "#FF9800")) ;; Value
          (13 . (md_storage :face font-lock-type-face)) ;; Enum
          (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
          (15 . md_closed_caption) ;; Snippet
          (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
          (17 . fa_file_text_o) ;; File
          (18 . md_refresh) ;; Reference
          (19 . fa_folder_open) ;; Folder
          (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
          (21 . (fa_square :face font-lock-constant-face)) ;; Constant
          (22 . (fa_cube :face font-lock-type-face)) ;; Struct
          (23 . fa_calendar) ;; Event
          (24 . fa_square_o) ;; Operator
          (25 . fa_arrows)) ;; TypeParameter
        )

  (unless gears-show-minor-modes
    (diminish 'company-mode)))

(defun gears-layers/auto_completion-description ()
  "Returns the description of the company layer."

  (eval "Provides autocompletion."))

(defun gears-layers/auto_completion-install ()
  (eval t))

(defun gears-layers/auto_completion-remove ()
  (eval t))

(gears-layer-defdepends auto_completion
                        :packages '(company company-quickhelp company-box))
