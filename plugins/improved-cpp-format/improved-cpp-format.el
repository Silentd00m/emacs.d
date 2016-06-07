;;; icf --- Improved indentation and formatting for C++.

;;; Commentary:

;;; Code:

(defvar icf-style "The used style.")

(defvar ics-style-list
  "List of all available styles."
  ((default . '((icf-indent-access-label . 0)
                (icf-inent-arglist-cont . 0)
                (icf-indent-arglist-close . 0)
                (icf-indent-class-body . 0)
                (icf-indent-block-close . 0)
                (icf-indent-block-body . 0)
                (icf-indent-block-open . 0)
                (icf-indent-class-open . 0)
                (icf-indent-class-body . 0)
                (icf-indent-class-close . 0)
                (icf-indent-innamespace . 0)
                (icf-indent-namespace-close . 0)
                (icf-indent-namespace-open . 0)
                (icf-indent-statement . 0)
                (icf-indent-statement-case-intro . 0)
                (icf-indent-statement-cont . 0)
                (icf-indent-topmost-intro . 0)
                (icf-indent-enum-class . 0)
                (icf-indent-template-open . 0)
                (icf-indent-template-close . 0)))))

(defun icf-new-style (style)
  ;; TODO : Add missing style options.

  (add-to-list 'icf-style-list 'style t))

(defun icf-set-style (name)
  "Selects the formatting style to be used. Must be added to the styles list"
  "using icf-new-style before being this function is called."
  (setq icf-current-style '(cdr (assoc name 'icf-style-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language element detection functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun icf-check-inside-template (pos)
  "Checks if POS is a class inside a template."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "template<.*>[\t\n ]*[(class)|(struct)]+"))))

(defun icf-check-inside-enum-class (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+")
                                        ;(looking-back "enum[ \t]+class[ \t]+")
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun icf-indent-access-label (langelem)
  "Indent access-label"
  '(cdr (assoc 'indent-access-label icf-current-style)))

(defun icf-indent-arglist-intro (langelem)
  "Indent arglist-intro"
  '(cdr (assoc 'indent-arglist-intro icf-current-style)))

(defun icf-indent-arglist-close (langelem)
  "Indent arglist-close"
  '(cdr (assoc 'indent-arglist-close icf-current-style)))

(defun icf-indent-class-close (langelem)
  "Indent class-close"
  '(cdr (assoc 'indent-class-close icf-current-style)))

(defun icf-indent-class-open (langelem)
  "Indent class-open"
  '(cdr (assoc 'indent-class-close icf-current-style)))

(defun icf-indent-block-close (langelem)
  "Indent block-close"
  '(cdr (assoc 'indent-block-close icf-current-style)))

(defun icf-indent-block-open (langelem))

(defun icf-indent-defun-block-intro (langelem))

(defun icf-indent-inclass (langelem))

(defun icf-indent-innamespace (langelem))

(defun icf-indent-namespace-close (langelem))

(defun icf-indent-namespace-open (langelem))

(defun icf-indent-statement (langelem))

(defun icf-indent-statement-case-intro (langelem))

(defun icf-indent-statement-cont (langelem))

(defun icf-indent-topmost-intro (langelem))

(defun icf-indent-topmost-intro-cont (langelem)
  "Indent topmost-intro-cont"
  (cond
   ((icf-check-inside-enum-class (c-langelem-pos langelem))
    ;; We're inside a enum-class.
    'icf-indent-enum-class-inner)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun icf-on ()
  "Initialization hook for icf"
  (interactive)

  (c-add-style "icf"
               '("linux"
                 (c-basic-offset . icf-format-basic-offset)
                 (c-offsets-alist
                  (access-label . icf-indent-access-label)
                  (arglist-close . icf-indent-arglist-close)
                  (arglist-intro . icf-indent-arglist-intro)
                  (block-close . icf-indent-block-close)
                  (block-open . icf-indent-block-open)
                  (case-label . icf-format-case-label)
                  (class-close . icf-indent-class-close)
                  (class-open . icf-indent-class-open)
                  (defun-block-intro . icf-format-defun-block-intro)
                  (defun-close . icf-format-defun-close)
                  (defun-open . icf-format-defun-open)
                  (inclass . icf-indent-inclass)
                  (inher-cont . icf-format-inher-cont)
                  (inher-intro . icf-format-inher-intro)
                  (inline-open . icf-format-inline-open)
                  (innamespace . icf-indent-innamespace)
                  (member-init-cont . icf-format-member-init-cont)
                  (member-init-intro . icf-format-member-init-intro)
                  (namespace-close . icf-indent-namespace-close)
                  (namespace-open . icf-indent-namespace-open)
                  (statement . icf-indent-statement)
                  (statement-block-intro . icf-indent-statement-block-intro)
                  (statement-case-intro . icf-indent-case-intro)
                  (statement-cont . icf-indent-statement-cont)
                  (substatement . icf-format-substatement)
                  (template-args-cont . icf-format-template-args-cont)
                  (topmost-intro . icf-format-topmost-intro)
                  (topmost-intro-cont . icf-format-topmost-intro-cont))))

  (c-set-style "icf"))

;;; icf.el ends here
