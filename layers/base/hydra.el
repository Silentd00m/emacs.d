(require 'hydra)

(load (concat gears-emacs-basepath
              "/functions/hydra"))

;;; Code:

(defvar gears-layers/base-hydra-list '())

(setq test-h nil)

;; (gears-hydra--generate test-hydra2
;;                        (make-gears-hydra :name "gears-hydra"
;;                                          :categories `(
;;                                                        ,(make-gears-hydra-category :title "GOTO"
;;                                                                                    :heads `(,(make-gears-hydra-head :key "c"
;;                                                                                                                     :text "Character"
;;                                                                                                                     :command 'goto-char)
;;                                                                                             ,(make-gears-hydra-head :key "l"
;;                                                                                                                     :text "Line"
;;                                                                                                                     :command 'goto-line)))
;;                                                        ,(make-gears-hydra-category :title "Git"
;;                                                                                    :heads `(,(make-gears-hydra-head :key "b"
;;                                                                                                                     :text "Blame")
;;                                      q                                                       ,(make-gears-hydra-head :key "s"
;;                                                                                                                     :text "Status")
;;                                                                                             ,(make-gears-hydra-head :key "L"
;;                                                                                                                     :text "Log")))
;;                                                        ,(make-gears-hydra-category :title "Graphics"
;;                                                                                    :heads `(,(make-gears-hydra-head :key "r"
;;                                                                                                                     :text "Rotate"))))))

(gears-defhydra test-hydra2
                `(,(make-gears-hydra-category :title "File"
                                              :heads `(,(make-gears-hydra-head :key "o"
                                                                               :text "Open"
                                                                               :command 'helm-find-files)
                                                       ,(make-gears-hydra-head :key "s"
                                                                               :text "Save"
                                                                               :command 'save-buffer)))))

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
