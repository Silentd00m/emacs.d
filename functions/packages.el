(load "~/.emacs.d/functions/gui")

(defun gears-package-install-packages (pkg-list)
  (generate-new-buffer-name "*gears-install*")
  (switch-to-buffer "*gears-install*")

  (read-only-mode t)

  (let ((inhibit-read-only t))
    (princ "Refreshing package repository cache before installation." (current-buffer)))

  (package-refresh-contents)

  (let ((gip-installed-package-count 0))
    (dolist (pkg pkg-list)
      (unless (package-installed-p pkg)
        (let ((inhibit-read-only t))
          (package-install pkg)))

      (setq gip-installed-package-count (+ gip-installed-package-count 1))

      (switch-to-buffer "*gears-install*")

      (let ((inhibit-read-only t))
        (erase-buffer)
        (princ "Please wait, installing packages... [" (current-buffer))
        (princ gip-installed-package-count (current-buffer))
        (princ "/" (current-buffer))
        (princ (length pkg-list) (current-buffer))
        (princ "]\n" (current-buffer))
        (gears-princ-progress-bar (current-buffer)
                                  (floor (* (/ (float gip-installed-package-count)
                                               (length pkg-list))
                                            100))))))

  (let ((inhibit-read-only t))
    (princ "\n\n" (current-buffer))
    (princ "Installation complete." (current-buffer))))

(defun gears-package-remove (package)
  (package-delete (gears-package-get-desc-vector package)))

(defun gears-package-get-name (package)
  (package-desc-name (elt (cdr (assoc package package-alist)) 0)))

(defun gears-package-get-version(package)
  (package-desc-version (elt (cdr (assoc package package-alist)) 0)))

(defun gears-package-get-desc-vector (package)
  (elt (cdr (assoc package package-alist)) 0))
