;; Source: https://emacs.stackexchange.com/questions/29872/how-many-bytes-are-in-the-region
(defun region-bytes (start end)
  "Return the number of bytes used by the region."
  (interactive "r")
  (message "Region has %d bytes."
           (- (bufferpos-to-filepos end 'exact)
              (bufferpos-to-filepos start 'exact))))

(global-set-key (kbd "M-+") #'region-bytes)