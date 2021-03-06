(defun gears-layers/markdown-init()
  (add-hook 'markdown-mode-hook '(lambda ()
                                   (markdown-preview-mode t))))

(defun gears-layers/markdown-remove()
  "Additional uninstall commands for the markdown-layer"

  t)

(defun gears-layers/markdown-install()
  "Additional install commands for the markdown-layer"

  t)

(defun gears-layers/markdown-description()
  "Returns the description of the Markdown layer."

  "General Markdown configuration and package layer.")

(gears-layer-defdepends markdown
                        :packages '(markdown-preview-mode))
