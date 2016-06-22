(require 'f)

(defun gears-layers/ycmd-init()
  (defgroup gears-layers/ycmd nil
    "YCMD Layer Configuration"
    :group 'gears)

  (defcustom gears-layer/ycmd-path (concat gears-emacs-basepath "/ycmd/ycmd")
    "Path to the YCMD server."

    :type 'directory
    :group 'gears-layers/ycmd)

  (defcustom gears-layer/ycmd-python "python3"
    "Python interpreter executable."

    :type '(file :must-match t)
    :group 'gears-layers/ycmd)

  (set-variable 'ycmd-server-command `(,gears-layer/ycmd-python ,gears-layer/ycmd-path))

  ;; Company setup

  (setq company-backends (remove-duplicates (push 'company-ycmd company-backends)))
  (setq company-ycmd-show-completion-kind t)

  ;; Flycheck setup
  (require 'flycheck-ycmd)
  (flycheck-ycmd-setup)

  ;; Make sure the flycheck cache sees the parse results
  (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)

  ;; Add the ycmd checker to the list of available checkers
  (add-to-list 'flycheck-checkers (remove-duplicates (push 'company-ycmd company-backends)))

  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))

  ;; Eldoc setup
  (require 'ycmd-eldoc)
  (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)

  ;; Add hook
  (global-ycmd-mode t))

(defun gears-layers/ycmd-description()
  "Returns a description of the YCMD layer."

  (eval "Provides the YCMD code-completion server."))

(defun gears-layers/ycmd-install()
  "Additional functions for installing the YCMD layer."
  ;; TODO : Add compilation of ycmd-server.
  (require 'git)

  (git-clone "https://github.com/Valloric/YouCompleteMe" (concat gears-emacs-basepath "/ycmd"))

  (eval t))

(defun gears-layers/ycmd-remove()
  "Additional functions for removing ycmd."

  ;; Delete the server directory.
  (when (f-exists? (concat gears-emacs-basepath "/" gears-layers/ycmd-basepath))
    (f-delete (concat gears-emacs-basepath "/" gears-layers/ycmd-basepath))))

(gears-layer-defdepends ycmd
                        :packages '(ycmd company-ycmd flycheck-ycmd ycmd-eldoc)
                        :layers '(company))
