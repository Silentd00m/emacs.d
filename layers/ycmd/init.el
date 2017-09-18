(require 'f)

(defun gears-layers/ycmd-init ()
  (defgroup gears-layers/ycmd nil
    "YCMD Layer Configuration"
    :group 'gears-layers)

  (defcustom gears-layers/ycmd-path (concat gears-emacs-basepath "/dep/ycmd/third_party/ycmd/ycmd/")
    "Path to the YCMD server."

    :type 'directory
    :group 'gears-layers/ycmd)

  (defcustom gears-layers/ycmd-python "python3"
    "Python interpreter executable."

    :type '(file :must-match t)
    :group 'gears-layers/ycmd)

    (defcustom gears-layers/ycmd-autoload-config t
      "If set to true, ycmd will always load .ycm-extra-conf files."

      :type 'boolean
      :group 'gears-layers/ycmd)

  (set-variable 'ycmd-server-command `(,gears-layers/ycmd-python ,gears-layers/ycmd-path))

  (when gears-layers/ycmd-autoload-config
    (setq ycmd-extra-conf-handler 'load))

  ;; Company setup

  (when (gears-layer-installed-p 'auto_completion)
    (require 'company-ycmd)
    (company-ycmd-setup)

    (add-hook 'c++-mode-hook #'(add-to-list 'company-backends 'company-ycmd)))

  ;; Flycheck setup
  (flycheck-ycmd-setup)

  ;; Make sure the flycheck cache sees the parse results
  (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)

  ;; Add the ycmd checker to the list of available checkers

  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))

  ;; Eldoc setup
  (require 'ycmd-eldoc)
  (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)

  ;; Add hook
  (global-ycmd-mode t))

(defun gears-layers/ycmd-description ()
  "Returns a description of the YCMD layer."

  (eval "Provides the YCMD code-completion server."))

(defun gears-layers/ycmd-install ()
  "Additional functions for installing the YCMD layer."
  ;; TODO : Add compilation of ycmd-server.
  (require 'git)

  (git-clone "https://github.com/Valloric/YouCompleteMe"
             (concat gears-emacs-basepath "/dep/ycmd"))

  t)

(defun gears-layers/ycmd-remove ()
  "Additional functions for removing ycmd."

  ;; Delete the server directory.
  (when (f-exists? (concat gears-emacs-basepath "/" gears-layers/ycmd-basepath))
    (f-delete (concat gears-emacs-basepath "/" gears-layers/ycmd-basepath))))

(gears-layer-defdepends ycmd
                        :packages '(ycmd company-ycmd flycheck-ycmd)
                        :layers '(auto_completion))
