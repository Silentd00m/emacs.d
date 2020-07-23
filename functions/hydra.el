(defvar dynhydra-list '())

(defun defdynhydra (name heads)
  "Define a new dynhydra."

  (map-put dynhydra-list name heads))

(defun dynhydra-add (hydra heads)
  "Add a new head to a dynhydra."

  (setf (cdr (assoc hydra dynhydra-list))
        (append (cdr (assoc hydra dynhydra-list))
                heads)))

(defmacro dynhydra-update (name heads)
  "Update a dynhydra."

  `(defhydra ,name (:color blue :hint nil)
     ,@(append heads '(("<ESC>" nil "Close Help" :exit t :column nil)))))

(defmacro dynhydra-generate (name)
  "Internal function that generates a dynhydra by name."

  `(dynhydra-update ,name ,(cdr (assoc name dynhydra-list))))

(defmacro dynhydra-open (hydra)
  "Updates and then opens a hydra."

  (eval `(dynhydra-generate ,hydra))
  (funcall-interactively (intern (concat (prin1-to-string hydra)
                                         "/body"))))
