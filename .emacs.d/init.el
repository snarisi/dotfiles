;; add directory for custom lisp
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; delete excess backup versions silently
(setq delete-old-versions -1 )

;; use version control
(setq version-control t )

;; make backups file even when in version controlled dir
(setq vc-make-backup-files t )

;; which directory to put backups file
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) )

;; don't ask for confirmation when opening symlinked file
(setq vc-follow-symlinks t )

;; transform backups file name
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) )

;; inhibit useless and old-school startup screen
(setq inhibit-startup-screen t )

;; silent bell when you make a mistake
(setq ring-bell-function 'ignore )

;; use utf-8 by default
(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8 )

;; sentence SHOULD end with only a point.
(setq sentence-end-double-space nil)

;; print a default message in the empty scratch buffer opened at startup
(setq initial-scratch-message "Hello World")

;; enable fullscreen
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)

;; set font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "Source Code Pro")))))

;; show line numbers
(global-linum-mode)
(setq linum-format "%4d ")
(require 'linum-off)

;; autoclose pairs
(electric-pair-mode 1)

;; use ipython for python shell
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize) ; guess what this one does ?

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package) ; guess what this one does too ?
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d5f17ae86464ef63c46ed4cb322703d91e8ed5e718bf5a7beb69dd63352b26b2" "c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "e91ca866d6cbb79786e314e0466f4f1b8892b72e77ed702e53bf7565e0dfd469" default)))
 '(package-selected-packages
   (quote
    (leuven-theme hydandata-light-theme flatui-theme flycheck exec-path-from-shell virtualenvwrapper jedi spaceline magit helm-projectile projectile helm auto-complete editorconfig spacemacs-theme doom-themes avy general use-package))))


(use-package python :ensure t
  :init
  (defun chdir-to-project-root ()
    "When opening a python shell while a projectile project is active, changes directory of python shell to project root"
    (when (projectile-project-name)
      (python-shell-send-string "import os")
      (python-shell-send-string (format "os.chdir('%s')" (projectile-project-root)))))
  (add-hook 'python-shell-first-prompt-hook #'chdir-to-project-root))

(use-package general :ensure t
  :config
  (general-define-key "C-'" 'avy-goto-word-1)
  (general-define-key "M-W" 'toggle-frame-fullscreen))

(use-package avy :ensure t
  :commands
  (avy-goto-word-1))

(use-package editorconfig :ensure t
  :config
  (editorconfig-mode 1))

(use-package spacemacs-theme :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

(use-package auto-complete :ensure t
  :config
  (ac-config-default)
  (add-to-list 'ac-sources 'ac-source-jedi-direct))

(use-package helm :ensure t)
(use-package projectile :ensure t
  :config
  (projectile-global-mode))
(use-package helm-projectile :ensure t)

(use-package magit :ensure t)

(use-package spaceline :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package exec-path-from-shell :ensure t
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-copy-env "DEV_MODE")
  (exec-path-from-shell-initialize)))

(use-package virtualenvwrapper :ensure t
  :config
  (defun workon-after-project-switch ()
    "Workon a project's virtualenv when switching a project."
    (message "now working on: %s" (projectile-project-name))
    (venv-workon (projectile-project-name)))
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (add-hook 'projectile-after-switch-project-hook #'workon-after-project-switch))

(use-package epc :ensure t)
(use-package jedi :ensure t
  :init
;;   ;; Global jedi config vars
;;   (defvar jedi-config:use-system-python nil
;;     "Will use system python and active environment for Jedi server.
;; May be necessary for some GUI environments (e.g., Mac OS X)")
;;   (defvar jedi-config:with-virtualenv nil
;;     "Set to non-nil to point to a particular virtualenv.")
;;   (defvar jedi-config:vcs-root-sentinel ".git")
;;   (defvar jedi-config:python-module-sentinel "__init__.py")
  :config
  ;; (when venv-current-name
  ;;   (setq jedi:server-args (list "--virtual-env" venv-current-name)))
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:get-in-function-call-delay 10000000)
  (setq jedi:environment-virtualenv (list (expand-file-name "~/.virtualenvs/")))
  (setq jedi:complete-on-dot t)
  (defun jedi-config:setup-keys ()
    (local-set-key (kbd "M-.") 'jedi:goto-definition)
    (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
    (local-set-key (kbd "M-?") 'jedi:show-doc)
    (local-set-key (kbd "M-/") 'jedi:get-in-function-call))
  (add-hook 'python-mode-hook 'jedi-config:setup-keys))

(use-package flycheck :ensure t
  :init
  (global-flycheck-mode))
