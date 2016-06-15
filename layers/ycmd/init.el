(require 'f)

(defun gears-layers/ycmd-init()
  (defgroup gears-layers/ycmd
    "YCMD Layer Configuration"
    :group 'gears)

  (defcustom gears-basepath
    :type ')

  (add-to-list company-backends 'company-ycmd)
  (setq company-ycmd-show-completion-kind t))

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

(setq gears-layers/ycmd-depends '((packages . (ycmd flycheck-ycmd company-ycmd))
                                  (layers . (flycheck company))))
