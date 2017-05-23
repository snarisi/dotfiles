;;; general-config -- Summary

;;; Commentary:
;; Custom Emacs configurations for me

;;; Code:
;; Change font
(custom-set-faces '(default ((t (:height 120 :family "Fira Mono for Powerline")))))

;; turn off sound
(setq ring-bell-function 'ignore)

;; enable fullscreen
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)

;; M-W to toggle fullscreen
(global-set-key (kbd "M-W") 'toggle-frame-fullscreen)

;; change autosave dir
(setq backup-directory-alist
   `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))

;; set theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn)

;; show line numbers
(global-linum-mode)
(setq linum-format "%4d ")
(require 'linum-off)

;; auto update changed files
(global-auto-revert-mode)

;; autoclose pairs
(electric-pair-mode 1)

;; git changes in margin
(global-git-gutter-mode +1)
(custom-set-variables
 '(git-gutter:update-interval 2))

;; add syntax highlighting for numbers in programming modes
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; editorconfig
(editorconfig-mode 1)

;; fix js switch statements
(setq js-switch-indent-offset 2)

;; use correct path in shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-copy-env "DEV_MODE")
  (exec-path-from-shell-initialize))

;; web mode jsx formatting in js files
(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

;; use ipython for python shell
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")


;; Global jedi config vars
(defvar jedi-config:use-system-python nil
  "Will use system python and active environment for Jedi server.
May be necessary for some GUI environments (e.g., Mac OS X)")
(defvar jedi-config:with-virtualenv nil
  "Set to non-nil to point to a particular virtualenv.")
(defvar jedi-config:vcs-root-sentinel ".git")
(defvar jedi-config:python-module-sentinel "__init__.py")


;; Projectile/Virualenv helper function
(defun workon-after-project-switch ()
  "Workon a project's virtualenv when switching a project."
  (message "now working on: %s" (projectile-project-name))
  (venv-workon (projectile-project-name))
  )


;; Package specific initialization
(add-hook
 'after-init-hook
 '(lambda ()

    ;; helm
    (require 'helm-config)

    ;; Projectile
    (require 'projectile)
    (projectile-global-mode)
    (add-hook 'projectile-after-switch-project-hook #'workon-after-project-switch)

    ;; Auto-complete
    (require 'auto-complete-config)
    (ac-config-default)
    (add-to-list 'ac-sources 'ac-source-jedi-direct)

    ;; Jedi
    (require 'jedi)
    (add-hook 'python-mode-hook 'jedi:setup)
    (defun jedi-config:setup-keys ()
      (local-set-key (kbd "M-.") 'jedi:goto-definition)
      (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
      (local-set-key (kbd "M-?") 'jedi:show-doc)
      (local-set-key (kbd "M-/") 'jedi:get-in-function-call))
    ;; Don't let tooltip show up automatically
    (setq jedi:get-in-function-call-delay 10000000)
    ;; Start completion at method dot
    (setq jedi:complete-on-dot t)
    ;; Use custom keybinds
    (add-hook 'python-mode-hook 'jedi-config:setup-keys)

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
    (global-set-key "\M-p" 'fastnav-sprint-forward)
    (global-set-key "\M-P" 'fastnav-sprint-backward)

    ;; no-easy-keys
    (require 'no-easy-keys)
    (no-easy-keys 1)

    ;; yasnippets
    (require 'yasnippet)
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode 1)

    ;; smart-mode-line/powerline
    (require 'powerline)
    (setq sml/theme 'powerline)
    (sml/setup)
    (powerline-default-theme)

    ;; winner mode
    (when (fboundp 'winner-mode)
      (winner-mode 1))

    ;; multiple cursors
    (require 'multiple-cursors)
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

    ;; virtualenvwrapper
    (require 'virtualenvwrapper)
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)

    ;; readline-complete (use autocomplete in shell buffer)
    (setq explicit-shell-file-name "bash")
    (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
    (setq comint-process-echoes t)
    (require 'readline-complete)
    (add-to-list 'ac-modes 'shell-mode)
    (add-hook 'shell-mode-hook 'ac-rlc-setup-sources)

    ))


(provide 'general-config)
;;; general-config.el ends here
