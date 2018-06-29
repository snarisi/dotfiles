(defun pyls-eldoc-function ()
  (when (symbol-at-point)
    (setq bounds (bounds-of-thing-at-point 'symbol))
    (setq request (lsp--make-request "textDocument/hover"))
    (symbol-at-point)))

(setq-local eldoc-documentation-function #'pyls-eldoc-function)

(provide 'pyls-eldoc-mode)
