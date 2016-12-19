(require 'elnode)
(require 'f)
(require 's)

(setq gears-config-ui-page-template
      (s-concat gears-emacs-basepath
                "/functions/config-ui/templates/page.html"))

(defun gears-config-ui-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon
                              '(("/layers/" . gears-config-ui-list-layers))))

(defun gears-config-ui-render (httpcon template-file replacements)
  (let* ((template (f-read-text template-file))
         (repl-tokens (mapcar (lambda (element)
                                (let ((key (car element))
                                      (val (cdr element)))
                                  (cons (s-concat "{{" key "}}") val)))
                              replacements))
         (output (s-replace-all repl-tokens template)))

    (elnode-send-html httpcon output)))

(defun gears-config-ui-list-layers (httpcon)
  (let ((output ""))
    (dolist (directory (f-directories (concat gears-emacs-basepath
                                              "/layers/")))
      (let ((layer-name (s-replace (concat gears-emacs-basepath
                                           "/layers/") "" directory)))
        (setq output (s-concat output "<li><a href=\"/layer/"
                               layer-name "/\">" layer-name "</a></li>\n"))))

    (setq output (concat "<ul>" output "</ul>"))

    (gears-config-ui-render httpcon
                            gears-config-ui-page-template
                            `(("body-data" . ,output)))))

(defun gears-config-ui-resources-handler (httpcon)
  (if (file-exists-p (s-concat gears-emacs-basepath
                              "/functions/config-ui/"
                              (elnode-http-pathinfo httpcon)))
      (elnode-send-file httpcon
                        (s-concat gears-emacs-basepath
                                  "/functions/config-ui/"
                                  (elnode-http-pathinfo httpcon)))
    (elnode-send-404 httpcon)))

(defun gears-config-ui-dash (httpcon)
  (let ((categories ""))
    (setq categories
          (s-concat categories
                    "<li><a href=\"/layers/\">Layers</a></li>"))

    (gears-config-ui-render httpcon
                            gears-config-ui-page-template
                            `(("body-data" . ,(s-concat "<ul>" categories "</ul>"))))))

(defun gears-config-ui-config-layer (httpcon)
  (let ((layer (nth 2 (s-split "/" (elnode-http-pathinfo httpcon)))))
    (if (file-exists-p (s-concat gears-emacs-basepath
                                 "/layers/"
                                 layer
                                 "/init.el"))
        ;; Load the module, in case it is not installed.

        (progn (load (concat gears-emacs-basepath
                             "/layers/"
                             layer
                             "/init"))

               (if (fboundp (intern (s-concat "gears-layers/"
                                              layer
                                              "-configure")))
                   (gears-config-ui-render httpcon
                                           gears-config-ui-page-template
                                           `(("body-data" .
                                             ,(funcall (intern
                                                        (s-concat "gears-layers/"
                                                                  layer
                                                                  "-configure"))
                                                       httpcon))))
                 (gears-config-ui-render httpcon
                                         gears-config-ui-page-template
                                         `(("body-data" .
                                            ,(s-concat "Layer does not provide"
                                                       " confiuration UI: "
                                                       layer))))))
      (gears-config-ui-render httpcon
                              gears-config-ui-page-template
                              `(("body-data" . ,(s-concat "Layer does not exist: "
                                                          layer)))))))

;; (defun gears-config-ui-dash (httpcon)
;;   (if (stringp (elnode-http-param httpcon "testvalue"))
;;       (gears-config-ui-render httpcon
;;                               gears-config-ui-page-template
;;                               `(("body-data" . ,(elnode-http-param
;;                                                  httpcon "testvalue")))))
;;   (gears-config-ui-render httpcon
;;                               gears-config-ui-page-template
;;                               `(("body-data" . "OK"))))

(defun gears-config-ui-handler (httpcon)
  (elnode-dispatcher httpcon '(("^/layers/" . gears-config-ui-list-layers)
                               ("^/layer/\\(.*\\)/" . gears-config-ui-config-layer)
                               ("^/resources/" . gears-config-ui-resources-handler)
                               ("^/" . gears-config-ui-dash))))

(defun gears-config-ui-start ()
  (elnode-start 'gears-config-ui-handler :port gears-webserver-port))
