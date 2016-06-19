;; -*- lexical-binding: t -*-

(require 'cl)
(require 's)

(cl-defstruct gears-hydra name categories)
(cl-defstruct gears-hydra-category title heads)
(cl-defstruct gears-hydra-head key text command)

;; Usage example:
;; (setq gears-hydra-h (make-gears-hydra :name "gears-hydra"
;;                               :categories `(
;;                                             ,(make-gears-hydra-category :title "GOTO"
;;                                                                  :heads `(,(make-gears-hydra-head :key "c"
;;                                                                                            :text "Character"
;;                                                                                            :command 'goto-character)
;;                                                                           ,(make-gears-hydra-head :key "l"
;;                                                                                            :text "Line"
;;                                                                                            :command 'goto-line)))
;;                                             ,(make-gears-hydra-category :title "Git"
;;                                                                  :heads `(,(make-gears-hydra-head :key "b"
;;                                                                                            :text "Blame")
;;                                                                           ,(make-gears-hydra-head :key "s"
;;                                                                                            :text "Status")
;;                                                                           ,(make-gears-hydra-head :key "L"
;;                                                                                            :text "Log")))
;;                                             ,(make-gears-hydra-category :title "Graphics"
;;                                                                  :heads `(,(make-gears-hydra-head :key "r"
;;                                                                                            :text "Rotate"))))))
;;
;; (gears-defhydra gears-hydra-def
;;                gears-hydra-h)

(defun gears-hydra-categories--max-length (hydra)
  "Returns the maximum number of heads inside any category of the given hydra."

  (let ((max-length 0))
    (dolist (category (gears-hydra-categories hydra))
      (when (< max-length (length (gears-hydra-category-heads category)))
        (setq max-length (length (gears-hydra-category-heads category)))))

    max-length))

(defun gears-hydra-category--max-headlength (category)
  "Returns the headlength (horizontal character count) of a category."

  (let ((max-length (+ (length (gears-hydra-category-title category)) 2)))
    (dolist (head (gears-hydra-category-heads category))
      (let ((head-length (+ (length (gears-hydra-head-key head))
                            (length "  ")
                            (length (gears-hydra-head-text head))
                            2)))

        (when (> head-length max-length)
          (setq max-length head-length))))

    (+ max-length 1)))

(defun gears-hydra-category-heads--nth (category n)
  "Returns the nth element from the category."

  (nth n (gears-hydra-category-heads category)))

(defun gears-hydra-category--format (category)
  (let ((output-string "")
        (max-headlength (gears-hydra-category--max-headlength category)))

    (let ((title (s-concat "^" (gears-hydra-category-title category) "^")))
      (when (< (length title) max-headlength)
        (setq title (s-append (concat (s-repeat (- max-headlength (length title)) " ")
                                     "\n")
                              title))

        (setq output-string title)))

    (setq output-string (s-append (concat (s-repeat max-headlength "-")
                                          "\n")
                                  output-string))

    (dolist (head (gears-hydra-category-heads category))
        (setq output-string (s-append (concat " _"
                                              (gears-hydra-head-key head)
                                              "_ "
                                              (gears-hydra-head-text head)
                                              (s-repeat (- max-headlength
                                                           (+ (length (gears-hydra-head-key head))
                                                              (length (gears-hydra-head-text head))
                                                              2))
                                                        " ")
                                              "\n") output-string)))

    ;; Return our output as list of lines
    (s-lines output-string)))

(defun gears-hydra--format (hydra)
  "Generates a formatted string listing all categories and heads."

  (let ((output-string "\n")
        (categories '())
        (category-widths '())
        (current-column 0)
        (max-category-length (gears-hydra-categories--max-length hydra)))

    (dolist (category (gears-hydra-categories hydra))
      (push (gears-hydra-category--format category) categories)
      (push (gears-hydra-category--max-headlength category) category-widths))

    (setq categories (nreverse categories))
    (setq category-widths (nreverse category-widths))

    ;; (princ (prin1-to-string category-widths) (current-buffer))

    (dotimes (row (+ max-category-length 2))
      (dotimes (col (length categories))
        (if (> (length (nth row (nth col categories))) 0)
            (setq output-string (s-append (concat (nth row (nth col categories)) " ")
                                          output-string))
          (setq output-string (s-append (s-repeat (+ (nth col category-widths) 1) " ")
                                        output-string)))

        (setq output-string (s-append " " output-string)))

      (setq output-string (s-append "\n" output-string)))

    output-string))

(defun gears-hydra--list-keys (hydra)

  (let ((return-list '()))
    (dolist (category (gears-hydra-categories hydra))
      (dolist (head (gears-hydra-category-heads category))
        (push `(,(eval (gears-hydra-head-key head)) ,(gears-hydra-head-command head)) return-list)))

    return-list))

(defmacro gears-defhydra (name hydra)
  (let ((docstring (eval (gears-hydra--format (eval hydra))))
        (heads (gears-hydra--list-keys (eval hydra))))
    `(defhydra ,name (:hint nil)
       ,docstring
       ,@heads
       ("<ESC>" nil "Close Help"))))
