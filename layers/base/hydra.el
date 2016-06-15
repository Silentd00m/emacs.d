(require 'hydra)

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
--------------------------------------
_l_ Go to Line         _g_ Git Status
_c_ Go to Character    _b_ Git Blame
"
  ("<ESC>" nil "Close Help")
  ("l" goto-line)
  ("c" goto-char)
  ("g" magit-status)
  ("b" magit-blame))
