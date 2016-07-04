;;; dynhydra --- Dynamic wrapper around hydra. -*- lexical-binding: t -*-

;;; Commentary:
;; Usage:
;; - Declare a dynamic hydra:
;; (defdynhydra example-hydra
;;   `(,(make-dynhydra-category :title "Example"
;;                              :heads `(,(make-dynhydra-head :key "q"
;;                                                            :text "Save and Exit"
;;                                                            :command 'save-buffers-kill-terminal)))))
;; - Add a category:
;;   (dynhydra--add-category 'example-hydra (make-dynhydra-category :title "Navigation"))
;; - Update the hydra
;;   (dynhydra--update gears-layers/base-hydra-m-p)

;;; Code:
;;* Requires
(require 'cl)
(require 's)
(require 'hydra)

(cl-defstruct dynhydra name categories)
(cl-defstruct dynhydra-category title heads)
(cl-defstruct dynhydra-head key text command exit)

(defvar dynhydra-list '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gears Hydra Category

(defun dynhydra-category--add-head (category head)
  "Adds a head to a category."

  (setf (dynhydra-category-heads category)
        (append (dynhydra-category-heads category) head)))

(defun dynhydra-category--remove-head (category head)
  "Removes the given head from the given category."

  (setf (dynhydra-category-heads category)
        (remove (dynhydra-category--get-head category head)
                (dynhydra-category-heads category))))

(defun dynhydra-category--get-head (category key)
  "Returns the head bound to the given key."

  (let ((match nil))
    (dolist (head (dynhydra-category-heads category))
      (when (string= (dynhydra-head-key head) key)
        (setq match head)))

    match))

(defun dynhydra-category--format (category)
  "Generate and format the info text of a category."

  (let ((output-string "")
        (max-headlength (dynhydra-category--max-headlength category)))

    (let ((title (s-concat " " (dynhydra-category-title category) " ")))
      (when (< (length title) max-headlength)
        (setq title (s-append (concat (s-repeat (- max-headlength (length title)) " ")
                                      "\n")
                              title))

        (setq output-string title)))

    (setq output-string (s-append (concat (s-repeat max-headlength "-")
                                          "\n")
                                  output-string))

    (dolist (head (dynhydra-category-heads category))
      (setq output-string (s-append (concat " _"
                                            (dynhydra-head-key head)
                                            "_ "
                                            (dynhydra-head-text head)
                                            (s-repeat (- max-headlength
                                                         (+ (length (dynhydra-head-key head))
                                                            (length (dynhydra-head-text head))
                                                            2))
                                                      " ")
                                            "\n") output-string)))

    ;; Return our output as list of lines
    (s-lines output-string)))

(defun dynhydra-category-heads--nth (category n)
  "Returns the nth element from the category."

  (nth n (dynhydra-category-heads category)))

(defun dynhydra-category--max-headlength (category)
  "Returns the headlength (horizontal character count) of a category."

  (let ((max-length (+ (length (dynhydra-category-title category)) 2)))
    (dolist (head (dynhydra-category-heads category))
      (let ((head-length (+ (length (dynhydra-head-key head))
                            (length "  ")
                            (length (dynhydra-head-text head))
                            2)))

        (when (> head-length max-length)
          (setq max-length head-length))))

    (+ max-length 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gears Hydra

(defun dynhydra-categories--max-length (hydra)
  "Returns the maximum number of heads inside any category of the given hydra."

  (let ((max-length 0))
    (dolist (category (dynhydra-categories hydra))
      (when (< max-length (length (dynhydra-category-heads category)))
        (setq max-length (length (dynhydra-category-heads category)))))

    max-length))

(defun dynhydra--format (hydra)
  "Generates a formatted string listing all categories and heads."

  (let ((output-string "\n")
        (categories '())
        (category-widths '())
        (current-column 0)
        (max-category-length (dynhydra-categories--max-length hydra)))

    (dolist (category (dynhydra-categories hydra))
      (push (dynhydra-category--format category) categories)
      (push (dynhydra-category--max-headlength category) category-widths))

    (setq categories (nreverse categories))
    (setq category-widths (nreverse category-widths))

    (dotimes (row (+ max-category-length 2))
      ;; Iterate over all categories. Categories will be displayed horizontally.
      (dotimes (col (length categories))
        (if (> (length (nth row (nth col categories))) 0)
            ;; If the category has enough heads for the current row, output the
            ;; head.
            (setq output-string (s-append (concat (nth row (nth col categories)) " ")
                                          output-string))
          ;; Else fill with whitespace.
          (setq output-string (s-append (s-repeat (+ (nth col category-widths) 1) " ")
                                        output-string)))

        (setq output-string (s-append " " output-string)))

      (setq output-string (s-append "\n" output-string)))

    output-string))

(defun dynhydra--add-category (hydra category)
  "Adds a catgory to a hydra."

  (setf (dynhydra-categories (cdr (assoc hydra dynhydra-list)))
        (append (dynhydra-categories (cdr (assoc hydra dynhydra-list)))
                `(,(make-dynhydra-category :title "Test"
                                           :heads `(,(make-dynhydra-head :key "L"
                                                                         :text "List")))))))

(defun dynhydra--remove-category (hydra category)
  "Removes a category from a hydra."

  (let ((category-list (dynhydra-categories (cdr (assoc hydra dynhydra-list)))))
    (dolist (cat category-list)
      (when (string= (dynhydra-category-title cat) category)
        (setf (dynhydra-categories (cdr (assoc hydra dynhydra-list)))
              (remove cat category-list))))))

(defun dynhydra--get-category (hydra category)
  "Returns the category with the given name from the hydra."

  (let ((match nil))
    (dolist (cat (dynhydra-categories (cdr (assoc hydra dynhydra-list))))
      (when (string= (dynhydra-category-title cat) category)
        (setq match cat)))

    match))

(defun dynhydra--list-keys (hydra)
  "Lists all key bindings in a hydra."

  (let ((return-list '()))
    (dolist (category (dynhydra-categories hydra))
      (dolist (head (dynhydra-category-heads category))
        (push `(,(dynhydra-head-key head) ,(dynhydra-head-command head) :exit ,(dynhydra-head-exit head)) return-list)))

    return-list))

(defmacro dynhydra--generate (name hydra)
  "Generates a hydra from the given name and dynhydra-struct"

  (let ((docstring (eval (dynhydra--format (eval hydra))))
        (heads (dynhydra--list-keys (eval hydra))))
    `(defhydra ,name (:hint nil)
       ,docstring
       ,@heads
       ("<ESC>" nil "Close Help"))))

(defmacro dynhydra--update (hydra)
  "Updates the hydra with the given name so new heads and categories are shown."

  `(dynhydra--generate ,hydra ,(cdr (assoc hydra dynhydra-list))))

(defmacro defdynhydra (name categories)
  "Adds the given hydra to the hydra-list and generates it.
Modifies it if already exists"

  (let ((hydra (make-dynhydra :name (prin1-to-string name)
                              :categories (eval categories))))

    (if (not (assoc name dynhydra-list))
        (setq dynhydra-list (push `(,name . ,hydra) dynhydra-list))
      (let ((item (assoc name dynhydra-list)))
        (setf (cdr item) hydra)))

    `(dynhydra--generate ,name ,hydra)))
