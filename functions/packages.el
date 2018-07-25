(load "~/.emacs.d/functions/gui")

(require 'dash)

(defun gears-package-install-packages (pkg-list)
  (get-buffer-create "*gears-install*")
  (switch-to-buffer "*gears-install*")

  (read-only-mode t)

  (let ((gip-installed-package-count 0))
    (dolist (pkg pkg-list)
      (package-install pkg)

      (setq gip-installed-package-count
            (+ gip-installed-package-count 1))

      (switch-to-buffer "*gears-install*")

      (let ((inhibit-read-only t))
        (switch-to-buffer "*gears-install*")
        (erase-buffer)
        (princ "Please wait, installing packages... ["
               (get-buffer-create "*gears-install*"))
        (princ gip-installed-package-count (get-buffer-create "*gears-install*"))
        (princ "/" (get-buffer-create "*gears-install*"))
        (princ (length pkg-list) (get-buffer-create "*gears-install*"))
        (princ "]\n" (get-buffer-create "*gears-install*"))
        (gears-princ-progress-bar
         (get-buffer-create "*gears-install*")
         (floor (* (/ (float gip-installed-package-count)
                      (length pkg-list))
                   100))))))

  (let ((inhibit-read-only t))
    (princ "\n\n" (get-buffer-create "*gears-install*"))
    (princ "Installation complete. Press q to close."
           (get-buffer-create "*gears-install*")))

  (local-set-key (kbd "q") 'kill-this-buffer))

(defun -gears-package-install-packages (pkg-list)
  (dolist (pkg pkg-list)
    (package-install pkg)))

(defun gears-package-is-outdated (package)
  "Return t if the installed version of PACKAGE is outdated."

  (if (member package package-selected-packages)
      (let* ((remote-version (package-desc-version
                              (cadr (assoc package package-archive-contents))))
             (local-version (pkg-info-package-version package)))
        (not (version-list-<= remote-version local-version)))
    nil))

(defun gears-package-list-outdated-packages ()
  "Return a list of outdated packages."

  (let ((outdated-package-list nil))
    (dolist (pkg package-selected-packages)
      (when (gears-package-is-outdated pkg)
        (add-to-list 'outdated-package-list pkg)))

    outdated-package-list))

(defun gears-package-update (package &optional background)
  (gears-package-install-packages `(,package) 0))

(defun gears-package-update-all-packages ()
  (interactive)

  (gears-package-install-packages (gears-package-list-outdated-packages)))

(defun gears-package-remove (package)
  (package-delete (gears-package-get-desc-vector package)))

(defun gears-package-get-name (package)
  (package-desc-name (elt (cdr (assoc package package-alist)) 0)))

(defun gears-package-get-version(package)
  (package-desc-version (elt (cdr (assoc package package-alist)) 0)))

(defun gears-package-get-desc-vector (package)
  (elt (cdr (assoc package package-alist)) 0))
