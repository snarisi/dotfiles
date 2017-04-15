;;; underscore --- Summary

;;; Commentary:

;;; Code:
(defun to-underscore ()
  (interactive)
  (progn (replace-regexp "\\([A-Z]\\)" "_\\1" nil
			 (region-beginning)
			 (region-end))
	 (downcase-region (region-beginning) (region-end))) )

(provide 'underscore)
;;; underscore.el ends here
