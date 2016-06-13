(load "~/.emacs.d/functions/gui")

(defun gears-install-packages(pkg-list)
  (generate-new-buffer-name "*gears-install*")
  (switch-to-buffer "*gears-install*")

  (read-only-mode 0)
  (princ "Refreshing package repository cache before installation." (current-buffer))
  (read-only-mode t)

  (package-refresh-contents)

  (let ((gip-installed-package-count 0))
    (dolist (pkg pkg-list)
      (unless (package-installed-p pkg)
        (package-install pkg))

      (setq gip-installed-package-count (+ gip-installed-package-count 1))

      (switch-to-buffer "*gears-install*")

      (read-only-mode 0)
      (erase-buffer)
      (princ "Please wait, installing packages... [" (current-buffer))
      (princ gip-installed-package-count (current-buffer))
      (princ "/" (current-buffer))
      (princ (length pkg-list) (current-buffer))
      (princ "]\n" (current-buffer))
      (gears-princ-progress-bar (current-buffer) (floor (* (/ (float gip-installed-package-count) (length pkg-list)) 100)))
      (read-only-mode t)))

   (read-only-mode 0)
   (princ "\n\n" (current-buffer))
   (princ "Installation complete." (current-buffer)))

(defun gears-package-remove(package)
  (package-delete (gears-package-get-desc-vector package)))

(defun gears-package-get-name(package)
  (package-desc-name (elt (cdr (assoc package package-alist)) 0)))

(defun gears-package-get-version(package)
  (package-desc-version (elt (cdr (assoc package package-alist)) 0)))

(defun gears-package-get-desc-vector(package)
  (elt (cdr (assoc package package-alist)) 0))
