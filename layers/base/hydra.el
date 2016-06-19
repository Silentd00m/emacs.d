(require 'hydra)

(cl-defstruct gears-hydra name categories)
(cl-defstruct gears-hydra-category title keys is-custom)
(cl-defstruct gears-hydra-key key command text length)

(make-gears-hydra :name "m-p")

(setq gears-hydra-list '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hydra

(defmacro gears-hydra-define (hname)
  "Defines a new hydra with the name given in hname."

  `(add-to-list ,(quote gears-hydra-list) '(make-gears-hydra :name ,tname))

  `(defhydra ,(intern hname) (:hint nil)
     ("<ESC>" nil "Close Help")))

(defun gears-hydra--format-categories (hydra)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Category

(defun gears-hydra-add-category (hydra category)
  "Add a new category to an existing hydra."

  (add-to-list (gears-hydra-categories hydra) category))

(defun gears-hydra-category--maxlength (category)
  "Returns the maximum line length in the current category"

  (let ((ghcm-maxlength 0))
    (dolist (c-key (gears-hydra-category-keys category))
      (when (> (gears-hydra-key-length c-key) ghcm-maxlength))))

  (eval ghcm-maxlength))

(defun gears-hydra-category--format (category)
  "Format a single category"

  (let ((ghcf-output (concat "\n ^" (gears-hydra-category-title category) "^\n"))
        (ghcf-maxlength (gears-hydra-category--maxlength category)))
    (setq ghcf-output (concat ghcf-output (make-string (+ ghcf-maxength 2) ?-)))

    (dolist (key (gears-hydra-category-keys category))
      (setq ghcf-output (concat ghcf-output (gears-hydra-key--format key))))

    (eval ghcf-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heads

;(gears-hydra-add "test")

(defhydra gears-layers/base-hydra-m-p (:hint nil)
  "
^Projectile^
----------------------------------
_p_ Open Project Buffers and Files
_s_ Switch Projects
_f_ Find File in Project
"
  ("<ESC>" nil "Close Help")
  ("p" helm-projectile)
  ("s" helm-projectile-switch-project)
  ("f" helm-projectile-find-file-dwim))

(defhydra gears-layers/base-hydra-m-g (:hint nil)
  "
^GOTO^                 ^GIT^
^-------------------^ ^--------------^
 _l_ Go to Line       _s_ Git Status
 _c_ Go to Character  _b_ Git Blame
                    _L_ Git Log
"
  ("<ESC>" nil "Close Help")
  ("l" goto-line)
  ("c" goto-char)
  ("s" magit-status)
  ("b" magit-blame)
  ("L" magit-log-all))
