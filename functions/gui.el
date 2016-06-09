(defun gears-princ-progress-bar (buffer percent)
  (princ (gears-ui-create-progress-bar buffer percent) buffer))

(defun gears-ui-create-progress-bar (buffer percent)
    (let ((gppb-window-width (window-body-width (get-buffer-window buffer)))
          (gppb-blocks (floor (* (/ (- (window-body-width (get-buffer-window buffer)) (float 11)) 100) percent))))

      (concat "  ["
              (make-string gppb-blocks ?█)

              ; Fix for negative string length if 100%
              (if (= (- gppb-window-width gppb-blocks 14) -3)
                  (make-string (- gppb-window-width gppb-blocks 11) ?░)
                (make-string (- gppb-window-width gppb-blocks 14) ?░))

              "] "
              (when (> 10 percent)
                " ")
              (when (> 100 percent)
                " ")
              (number-to-string percent)
              "\%")))
