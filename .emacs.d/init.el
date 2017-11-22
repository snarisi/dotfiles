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
(setq initial-scratch-message "Hey")

;; enable fullscreen
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)

;; to get it to work in client mode
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

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
;; use system ipython - make sure it doesn't break venv
;; (setq python-shell-interpreter "/usr/local/bin/ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")
;; (setenv "IPY_TEST_SIMPLE_PROMPT" "1")

;; use C-c C-y to yank while in ansi-term
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-y") 'term-paste))

"Configure use-package and install it if it isn't there."
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

(require 'use-package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (virtualenvwrapper company-tern xref-js2 js2-refactor rjsx-mode winner-mode flycheck multiple-cursors markdown-mode company-statistics yasnippet swoop exec-path-from-shell helm-smex helm-git-grep helm-ag magit spaceline spacemacs-theme expand-region general helm-projectile projectile helm company-anaconda company pyvenv anaconda-mode use-package)))
 '(safe-local-variable-values
   (quote
    ((eval progn
	   (add-to-list
	    (quote exec-path)
	    (concat
	     (locate-dominating-file default-directory ".dir-locals.el")
	     "node_modules/.bin/")))))))


;; General stuff
(use-package general :ensure t
  :config
  (general-define-key "M-W" 'toggle-frame-fullscreen))

(global-set-key (kbd "C-c s") 'shell)

(use-package expand-region :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package exec-path-from-shell :ensure t
  :config
  (message "exec-path-from-shell started")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "DEV_MODE")
    (exec-path-from-shell-initialize)))

(use-package winner-mode :ensure t
  :init
  (winner-mode t))

;; Themes
(use-package spacemacs-theme :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t)
  (defun load-theme-for-client (_)
    (load-theme 'spacemacs-dark t))
  (add-to-list 'after-make-frame-functions #'load-theme-for-client))

(use-package spaceline :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package multiple-cursors :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package flycheck :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (global-flycheck-mode))

;; Auto-Complete
(use-package company :ensure t
  :init
  (progn
    (setq company-idle-delay 0.1
          company-minimum-prefix-length 2
          company-require-match nil
          company-dabbrev-ignorecom-case nil
          company-dabbrev-downcase nil))
  :config
  (global-company-mode))

(use-package company-statistics :ensure t
  :defer t
  :init
  (progn
    (setq company-statistics-file "~/.emacs.d/company-statistics-cache.el")
    (add-hook 'company-mode-hook 'company-statistics-mode)))

(use-package yasnippet :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"
	  "~/.emacs.d/custom_snippets"))
  (yas-global-mode 1))

;; Project, navigation, etc.
(use-package helm :ensure t)
(use-package projectile :ensure t
  :config
  (projectile-mode))
(use-package helm-projectile :ensure t)

(use-package helm-git-grep :ensure t
  :config
  (global-set-key (kbd "C-c g") 'helm-git-grep)
  ;; Invoke `helm-git-grep' from isearch.
  (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
  ;; Invoke `helm-git-grep' from other helm.
  (eval-after-load 'helm
    '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm)))

(use-package helm-smex :ensure t
  :config
  (global-set-key [remap execute-extended-command] #'helm-smex)
  (global-set-key (kbd "M-X") #'helm-smex-major-mode-commands))

(use-package swoop :ensure t
  :config
  (setq swoop-font-size-change: nil)
  (global-set-key (kbd "C-c o")   'swoop)
  (global-set-key (kbd "C-S-c o") 'swoop-multi)
  (global-set-key (kbd "C-M-s")   'swoop-pcre-regexp)
  (global-set-key (kbd "C-S-o") 'swoop-back-to-last-position))

;; Org mode
(use-package org :ensure t
  :config
  (add-to-list 'org-export-backends 'md))

;; Python stuff
(use-package python :ensure t
  :init
  (defun chdir-to-project-root ()
    "When opening a python shell while a projectile project is active, changes directory of python shell to project root"
    (when (projectile-project-name)
      (python-shell-send-string "import os")
      (python-shell-send-string (format "os.chdir('%s')" (projectile-project-root)))))
  (add-hook 'python-shell-first-prompt-hook #'chdir-to-project-root))

(use-package anaconda-mode :ensure t
  :defer t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda :ensure t
  :init
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

;; figure out which of these works better
;; (use-package pyvenv :ensure t
;;   :config
;;   (defun workon-after-project-switch ()
;;     "Workon a project's virtualenv when switching a project."
;;     (message "now working on: %s" (projectile-project-name))
;;     (pyvenv-workon (projectile-project-name)))
;;   (add-hook 'projectile-after-switch-project-hook #'workon-after-project-switch))

(use-package virtualenvwrapper :ensure t
  :config
  (defun workon-after-project-switch ()
    "Workon a project's virtualenv when switching a project."
    (message "now working on: %s" (projectile-project-name))
    (venv-workon (projectile-project-name)))
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (add-hook 'projectile-after-switch-project-hook #'workon-after-project-switch))

;; Javascript stuff
(use-package rjsx-mode :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package js2-refactor :ensure t)

(use-package xref-js2 :ensure t
  :init
  (add-hook 'js2-mode-hook
	    (lambda ()
	      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  (define-key js2-mode-map (kbd "M-.") nil)
  (define-key js2-mode-map (kbd "M-*") 'xref-pop-marker-stack)
  (define-key js2-mode-map (kbd "M-r") 'xref-find-references))

(use-package company-tern :ensure t
  :config
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'rjsx-mode-hook (lambda () (tern-mode))))

;; Magit
(use-package magit :ensure t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(put 'narrow-to-region 'disabled nil)

(require 'server)
(unless (server-running-p) (server-start))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
