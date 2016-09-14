;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces

(defface gears-ui-header-bar-face '((t (:background "black"
                                                    :box '(:line-width 2 :color "white" :style nil))))
  "Gears Header Bar Face")

(defface gears-ui-progress-bar-face '((t (:box '(:line-width 2 :color "white" :style nil))))
  "Gears Progress Bar Face")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun gears-princ-progress-bar (buffer percent)
  (princ (gears-ui-create-progress-bar buffer percent) buffer))

(defun gears-ui-create-progress-bar (buffer percent)
    (let ((gppb-window-width (window-body-width (get-buffer-window buffer)))
          (gppb-blocks (floor (* (/ (- (window-body-width (get-buffer-window buffer)) (float 11)) 100) percent))))

      (concat "   "
              (propertize (concat (make-string gppb-blocks ?█)

                                  ;; Fix for negative string length if 100%
                                  (if (= (- gppb-window-width gppb-blocks 14) -3)
                                      (make-string (- gppb-window-width gppb-blocks 11) ?░)
                                    (make-string (- gppb-window-width gppb-blocks 14) ?░)))
                          'face "gears-ui-progress-bar-face")

                "  "
                (when (> 10 percent)
                  " ")
                (when (> 100 percent)
                  " ")
                (number-to-string percent)
              "\%")))

(defun gears-ui-create-header-bar(text buffer)
  (concat (propertize (concat "   "
                              text
                              (make-string (- (window-body-width (get-buffer-window buffer)) 4 (length text)) ?\ ))
                      'face "gears-ui-header-bar-face")
          "\n"))
