;; add directory for custom lisp
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; increase garbage collection threshold
;; copied from spacemacs: https://github.com/syl20bnr/spacemacs/blob/582f9aa45c2c90bc6ec98bccda33fc428e4c6d48/init.el#L17
;; (setq gc-cons-threshold 100000000)

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

;; Show line numbers
(global-linum-mode)
(setq linum-format "%4d ")
(require 'linum-off)

;; autoclose pairs
(electric-pair-mode 1)
(add-hook 'inferior-python-mode-hook
	  (lambda ()
	    (setq-local electric-pair-pairs '(append electric-pair-pairs (?\' . ?\')))))  ;; autoclose single quote
(add-hook 'shell-mode-hook
	  (lambda ()
	    (setq-local electric-pair-pairs '(append electric-pair-pairs (?\' . ?\')))))  ;; autoclose single quote

;; show matching parentheses
(show-paren-mode 1)

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
    (bash-completion company-shell flyspell-correct-helm writeroom-mode olivetti memory-usage helm-gtags disable-mouse disable-moouse zenburn-theme doom-themes atom-one-dark-theme atom-dark-theme dracula-theme transpose-frame tao-theme arjen-grey-theme material-theme nimbus-theme grayscale-theme iedit editorconfig fastnav virtualenvwrapper company-tern xref-js2 js2-refactor rjsx-mode winner-mode flycheck multiple-cursors markdown-mode company-statistics yasnippet swoop exec-path-from-shell helm-smex helm-git-grep helm-ag magit spaceline spacemacs-theme expand-region general helm-projectile projectile helm company-anaconda company pyvenv anaconda-mode use-package)))
 '(safe-local-variable-values
   (quote
    ((eval progn
	   (add-to-list
	    (quote exec-path)
	    (concat
	     (locate-dominating-file default-directory ".dir-locals.el")
	     "node_modules/.bin/")))))))


;; General stuff
(eval-after-load 'autorevert
  (lambda ()
    (diminish 'auto-revert-mode)))

(use-package general :ensure t
  :config
  (general-define-key "M-W" 'toggle-frame-fullscreen))

(global-set-key (kbd "C-c s") 'shell)

(use-package editorconfig :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package expand-region :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package exec-path-from-shell :ensure t
  :config
  (message "exec-path-from-shell started")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "DEV_MODE")
    (exec-path-from-shell-initialize)))

;; not on an mirror?
(use-package winner-mode
  :init
  (winner-mode t))

(use-package transpose-frame :ensure t
  :config
  (global-set-key (kbd "C-c t") 'transpose-frame)
  (global-set-key (kbd "C-c f") 'flip-frame)
  (global-set-key (kbd "C-c l") 'flop-frame))

(use-package fastnav :ensure t
  :config
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
  ;; (global-set-key "\M-k" 'fastnav-delete-char-forward)
  ;; (global-set-key "\M-K" 'fastnav-delete-char-backward)
  (global-set-key "\M-m" 'fastnav-mark-to-char-forward)
  (global-set-key "\M-M" 'fastnav-mark-to-char-backward))

(defun to-underscore ()
  (interactive)
  (progn (replace-regexp "\\([A-Z]\\)" "_\\1" nil (region-beginning) (region-end)) (downcase-region (region-beginning) (region-end))))

(use-package disable-moouse :ensure t
  :diminish disable-mouse-global-mode
  :init
  (global-disable-mouse-mode)
  (diminish 'disable-mouse-global-mode))

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
  :diminish flycheck-mode
  :config
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (global-flycheck-mode))

;; Auto-Complete
(use-package company :ensure t
  :diminish company-mode
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

(use-package bash-completion :ensure t
  :init
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
	    'bash-completion-dynamic-complete))

(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"
	  "~/.emacs.d/custom_snippets"))
  (yas-global-mode 1))

;; Project, navigation, etc.
(use-package helm :ensure t
  :config
  (global-set-key (kbd "C-x b") 'helm-mini)
  (setq helm-split-window-in-side-p t))

(use-package projectile :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t)
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
  (setq swoop-window-split-current-window: t)
  (global-set-key (kbd "C-c o")   'swoop)
  (global-set-key (kbd "C-S-c o") 'swoop-multi)
  (global-set-key (kbd "C-M-s")   'swoop-pcre-regexp)
  (global-set-key (kbd "C-S-o") 'swoop-back-to-last-position))

;; Org mode
(use-package org :ensure t
  :config
  (add-to-list 'org-export-backends 'md))

;; Python stuff
(use-package python
  :init
  (defun chdir-to-project-root ()
    "When opening a python shell while a projectile project is active, changes directory of python shell to project root"
    (when (projectile-project-name)
      (python-shell-send-string "import os")
      (python-shell-send-string (format "os.chdir('%s')" (projectile-project-root)))))
  (add-hook 'python-shell-first-prompt-hook #'chdir-to-project-root))

(use-package anaconda-mode :ensure t
  :diminish anaconda-mode
  :diminish eldoc-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (define-key anaconda-mode-map (kbd "M-,") 'anaconda-mode-go-back))

(use-package company-anaconda :ensure t
  :init
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

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
  (setq js2-basic-offset 2)
  (define-key js2-mode-map (kbd "M-.") nil)
  (define-key js2-mode-map (kbd "M-*") 'xref-pop-marker-stack)
  (define-key js2-mode-map (kbd "M-r") 'xref-find-references))

(use-package company-tern :ensure t
  :config
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'rjsx-mode-hook (lambda () (tern-mode))))

;; C# stuff
(use-package omnisharp :ensure t
  :init
  (defun my-csharp-mode-setup ()
    (require 'omnisharp-settings)
    (require 'omnisharp-utils)
    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq tab-width 4)
    (setq evil-shift-width 4)
    (local-set-key (kbd "C-c C-c") 'recompile)
    (define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
    (define-key omnisharp-mode-map (kbd "M-r") 'omnisharp-helm-find-usages))
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
  (eval-after-load 'company '(add-to-list 'company-backends 'company-omnisharp)))

;; Magit
(use-package magit :ensure t
  :init
  (global-set-key (kbd "C-c m") 'magit-status))

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(put 'narrow-to-region 'disabled nil)

;; writing stuff
(use-package olivetti :ensure t
  :config
  (olivetti-set-width 120))

(require 'server)
(unless (server-running-p) (server-start))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
