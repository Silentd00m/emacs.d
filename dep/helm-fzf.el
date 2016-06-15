(provide 'helm-fzf)

(defvar helm-fzf-source
  (helm-build-async-source "fzf"
    :candidates-process 'helm-fzf--do-candidate-process
    :nohighlight t
    :requires-pattern 2
    :candidate-number-limit 9999))

(defun helm-fzf--do-candidate-process ()
  (let* ((cmd-args `("fzf" "-x" "-f" ,helm-pattern))
         (proc (apply #'start-file-process "helm-fzf" nil cmd-args)))
    (prog1 proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (helm-process-deferred-sentinel-hook
          process event (helm-default-directory)))))))

(defun helm-fzf-home()
  (interactive)
  (let ((default-directory "~/"))
	(find-file
	 (concat "~/" (helm :sources '(helm-fzf-source)
						:buffer "*helm-fzf*")))))

(defun helm-fzf-directory(directory)
  (interactive "D")
  (let ((default-directory directory))
    (helm :sources '(helm-fzf-source)
          :buffer "*helm-fzf*")))

(defun helm-fzf()
  (interactive)

  (if (fboundp #'projectile-project-root)
      (let ((default-directory (condition-case err
                                   (projectile-project-root)
                                 (error
                                  default-directory))))
        (helm :sources '(helm-fzf-source)
              :buffer "*helm-fzf*"))
    (call-interactively 'helm-fzf-directory)))
