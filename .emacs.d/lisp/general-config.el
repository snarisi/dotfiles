(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; M-W to toggle fullscreen
(global-set-key (kbd "M-W") 'toggle-frame-fullscreen)

;; change autosave dir
(setq backup-directory-alist
   `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))

;; theme dir
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn)

;; show line numbers
(global-linum-mode)
(setq linum-format "%4d ")
(require 'linum-off)

;; auto update changed files
(global-auto-revert-mode)

;; hide menu bar
(menu-bar-mode -1)

;; autoclose pairs
(electric-pair-mode 1)

;; git changes in margin
(global-git-gutter-mode +1)
(custom-set-variables
 '(git-gutter:update-interval 2))

;; helm
(require 'helm-config)

;; toggle fullscreen
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                           'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)

;; highlight numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; fastnav
(require 'fastnav)
(global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)
(global-set-key "\M-s" 'fastnav-jump-to-char-forward)
(global-set-key "\M-S" 'fastnav-jump-to-char-backward)
(global-set-key "\M-r" 'fastnav-replace-char-forward)
(global-set-key "\M-R" 'fastnav-replace-char-backward)
(global-set-key "\M-i" 'fastnav-insert-at-char-forward)
(global-set-key "\M-I" 'fastnav-insert-at-char-backward)
(global-set-key "\M-j" 'fastnav-execute-at-char-forward)
(global-set-key "\M-J" 'fastnav-execute-at-char-backward)
(global-set-key "\M-k" 'fastnav-delete-char-forward)
(global-set-key "\M-K" 'fastnav-delete-char-backward)
(global-set-key "\M-m" 'fastnav-mark-to-char-forward)
(global-set-key "\M-M" 'fastnav-mark-to-char-backward)
;(global-set-key "\M-p" 'fastnav-sprint-forward)
;(global-set-key "\M-P" 'fastnav-sprint-backward)

;; disable "easy keys"
(require 'no-easy-keys)
(no-easy-keys 1)

;; yasnippets
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
;; (eval-after-load 'yasnippet
;;   '(progn
;;      (define-key yas-keymap (kbd "TAB") nil)
;;      (define-key yas-keymap (kbd "C-TAB") 'yas-expand)))

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; editorconfig
(editorconfig-mode 1)

;; fix js switch statements
(setq js2-indent-switch-body t)

(provide 'general-config)
;;; general-config.el ends here
