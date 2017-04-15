;;; package --- Summary
;; Sets keybindings to switch emacs and tmux panes

;;; Commentary:

;;; Code:
(defun move-window-or-tmux-pane(dir tmux-cmd)
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
    nil ;; do nothing, move w/ emacs
    (shell-command tmux-cmd))  ;; send command to tmux
)

(global-set-key "\M-P"
  '(lambda () (interactive) (move-window-or-tmux-pane "up"  "tmux select-pane -U")))
(global-set-key "\M-N"
  '(lambda () (interactive) (move-window-or-tmux-pane "down"  "tmux select-pane -D")))
(global-set-key "\M-F"
  '(lambda () (interactive) (move-window-or-tmux-pane "right" "tmux select-pane -R")))
(global-set-key "\M-B"
  '(lambda () (interactive) (move-window-or-tmux-pane "left"  "tmux select-pane -L")))

(provide 'navigation)
;;; navigation.el ends here
