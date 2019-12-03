(defvar dynhydra-list '())
;; (setq --dynhydra-list '())

(defun defdynhydra (name heads)
  (map-put dynhydra-list name heads))

(defun dynhydra-add (name head)
  (setf (cdr (assoc 'vi dynhydra-list)) (append (cdr (assoc name dynhydra-list))
                                                '(head))))

(defmacro dynhydra-update (name heads)
  `(defhydra ,name (:color pink) ,@heads))

(defmacro dynhydra-generate (name)
  (eval `(dynhydra-update ,name ,(cdr (assoc name dynhydra-list)))))

(defmacro dynhydra-open (name)
  "Updates and then opens a hydra."

  (dynhydra-generate name)
  (funcall-interactively (intern (concat (prin1-to-string name)
                           "/body"))))
