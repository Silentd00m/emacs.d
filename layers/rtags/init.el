;; -*- lexical-binding: t -*-

;;; Code:

(defun gears-layers/rtags-get-rtags-directory ()
  "Return the directory of the newest rtags version."

  (concat gears-emacs-basepath
          "/dep/rtags/"
          (nth 1 (cl-sort (directory-files (concat gears-emacs-basepath
                                                   "/dep/rtags/"))
                          'string-greaterp
                          :key 'downcase))))

(defun gears-layers/rtags-hydra-add ()
  (dynhydra-category--add-head (dynhydra--get-category 'gears-layers/base-hydra-m-g "GOTO")
                               `(,(make-dynhydra-head :key "d"
                                                      :text "Definition"
                                                      :command 'rtags-find-symbol-at-point
                                                      :exit t
                                                      :condition (lambda ()
                                                                   (or (eq major-mode 'c++-mode)
                                                                       (eq major-mode 'c-mode))))))
  (unless (dynhydra--get-category 'gears-layers/base-hydra-m-r "Refactor")
    (dynhydra--add-category 'gears-layers/base-hydra-m-r
                            `(,(make-dynhydra-category :title "Refactor"))))

  (dynhydra-category--add-head (dynhydra--get-category 'gears-layers/base-hydra-m-r
                                                       "Refactor")
                               `(,(make-dynhydra-head :key "S"
                                                      :text "Rename Symbol"
                                                      :command 'rtags-rename-symbol
                                                      :exit t
                                                      :condition (lambda()
                                                                   (or (eq major-mode 'c++-mode)
                                                                       (eq major-mode 'c-mode)))))))

(defun gears-layers/rtags-init ()
  (defgroup gears-layers/rtags nil
    "Rtags layer."

    :group 'gears-layers)

  (defcustom gears-layers/rtags-provide-completion t
    "Use rtags for completion wherever possible."

    :type 'boolean
    :group 'gears-layers/rtags)

  (defcustom gears-layers/rtags-provide-error-checking t
    "Use rtags for error checking wherever possible."

    :type 'boolean
    :group 'gears-layers/rtags)

  (unless (f-exists-p (concat gears-emacs-basepath "/dep/rtags"))
    (gears-layers/rtags-install))

  (require 'flycheck-rtags)
  (require 'helm-rtags)

  (setq rtags-use-helm t)

  ;; Add goto-definition for c-modes.
  (gears-layers/rtags-hydra-add)

  (when (and (gears-layer-installed-p 'auto_completion)
             gears-layers/rtags-provide-completion)
    (setq rtags-completions-enabled t)
    (add-to-list 'company-backends 'company-rtags))

  (when gears-layers/rtags-provide-error-checking
    (add-hook 'c-mode-common-hook
              #'(lambda ()
                  (flycheck-select-checker 'rtags)
                  (setq-local flycheck-highlighting-mode nil)
                  (setq-local flycheck-check-syntax-automatically nil))))

  (setq rtags-path (concat (gears-layers/rtags-get-rtags-directory)
                           "/bin"))

  (when (gears-layer-installed-p 'cmake)
    (require 'cmake-ide)

    (setq cmake-ide-rdm-executable (concat (gears-layers/rtags-get-rtags-directory)
                                           "/bin/rdm")
          cmake-ide-rdm-rc-path (concat (gears-layers/rtags-get-rtags-directory)
                                        "/bin/rdm")))

  ;; Auto-start rdm and rc when opening a C++ buffer
  (rtags-start-process-unless-running))

(defun gears-layers/rtags-description ()
  "Client/Server indexer for C++.")

(defun gears-layers/rtags-install ()
  "Blerp."

  (require 'rtags)

  (rtags-install (concat gears-emacs-basepath "/dep/rtags")))

(gears-layer-defdepends rtags
                        :packages '(rtags
                                    helm-rtags
                                    flycheck-rtags)
                        :layers '(cpp))
