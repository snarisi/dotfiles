(define-minor-mode number-font-lock-mode
  "Minor mode for highlighting numbers."
  :group 'number-font-lock
  :lighter " :)"
  (cond
    (number-font-lock-mode
      (when (not (or font-lock-mode global-font-lock-mode))
        (font-lock-mode 1))
      (font-lock-add-keywords nil
        (list (list "\\([0-9]+\\)" '(0 'number-font-lock-face t)) ))
      (message "Turned ON `number-font-lock-mode`."))
    ((not number-font-lock-mode)
      (font-lock-remove-keywords nil
        (list (list "\\([0-9]+\\)" '(0 'number-font-lock-face t)) ))
      (font-lock-fontify-buffer)
      (message "Turned OFF `number-font-lock-mode`."))))

(defgroup number-font-lock nil
  "Highlight numbers."
  :version "0.1"
  :group 'number-font-lock)

(defface number-font-lock-face
  '((t (:foreground "#87AF5F" :background "#000000")))
  "Face for `number-font-lock-face`."
  :group 'number-font-lock)

(defun turn-on-number-font-lock-mode ()
(interactive)
  (number-font-lock-mode 1))

(defun turn-off-number-font-lock-mode ()
(interactive)
  (number-font-lock-mode -1))

(define-globalized-minor-mode global-number-font-lock-mode
  number-font-lock-mode turn-on-number-font-lock-mode)

;; (global-number-font-lock-mode)

;; (add-hook 'python-mode-hook #'number-font-lock-mode)

(provide 'number-font-lock)
;;; number-font-lock ends here
