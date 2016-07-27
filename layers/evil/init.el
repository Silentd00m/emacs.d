(defun gears-layers/evil-init()
  (defcustom gears-use-evil nil
    "Use evil-mode instead of Gears' cua-mode by default."
    :type 'boolean
    :group 'gears-keyboard)

  (when gears-use-evil
    (evil-mode t))

  ;; Overwrite state maps with global keymap.
  (dolist (i gears-global-keymap)
    (dolist (map '(evil-normal-state-map
                   evil-insert-state-map
                   evil-motion-state-map
                   evil-visual-state-map))
      (unless (string= (car i) "<escape>")
        (define-key (eval map) (kbd (car i)) (cdr i))))))

(defun gears-layers/evil-description()
  "Evil-mode support layer.")

(defun gears-layers/evil-install ()
  "Additional install commands for evil layer.")

(gears-layer-defdepends evil
                        :packages '(evil
                                    powerline-evil
                                    evil-nerd-commenter))
