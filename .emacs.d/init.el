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
;; 4 space tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

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

;; auto generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#073642")
 '(company-quickhelp-color-foreground "#839496")
 '(custom-safe-themes
   (quote
    ("d5f17ae86464ef63c46ed4cb322703d91e8ed5e718bf5a7beb69dd63352b26b2" "c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "e91ca866d6cbb79786e314e0466f4f1b8892b72e77ed702e53bf7565e0dfd469" default)))
 '(package-selected-packages
   (quote
    (company-irony company-c-headers pdf-tools irony speed-type company-anaconda company-quickhelp ac-anaconda anaconda-mode auto-complete-c-headers auto-complete-clang-async projectile-speebar ggtags helm-gtags jedi auto-complete company-jedi ycmd epc web-mode company-ycmd company redis pyvenv ycmd-eldoc company-elisp helm-rtags rtags-helm rtags yasnippet multiple-cursors fastnav emmet-mode expand-region leuven-theme hydandata-light-theme flatui-theme flycheck exec-path-from-shell virtualenvwrapper spaceline magit helm-projectile projectile helm editorconfig spacemacs-theme doom-themes avy general use-package)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#839496")
 '(pos-tip-use-relative-coordinates nil)
 '(winner-mode t))
(put 'narrow-to-region 'disabled nil)

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
  :init
  ;; (local-set-key (kbd "M-.") 'anaconda-mode-find-definitions)
  ;; (local-set-key (kbd "M-,") 'anaconda-mode-go-back)
  ;; (local-set-key (kbd "M-?") 'anaconda-mode-show-doc)
  ;; (local-set-key (kbd "M-/") 'anaconda-mode-find-assignments)
  ;; (local-set-key (kbd "M-r") 'anaconda-mode-find-references)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (defun init-anaconda-eldoc-mode ()
    (progn
      (eldoc-mode)
      (anaconda-eldoc-mode)))
  (add-hook 'python-mode-hook 'init-anaconda-eldoc-mode))

(use-package company-anaconda :ensure t
  :init
  ;; for some reason quickhelp doesn't work unless this is here
  (defun my-annotation-function (candidate)
    (let ((description (get-text-property 0 'description candidate)))
      (when description
        (concat "<" description ">"))))

  (setq company-anaconda-annotation-function 'my-annotation-function)
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

(use-package company :ensure t
  :init
  (progn
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil))
  :config
  (progn
    (defun company-complete-common-or-cycle-backward ()
      "Complete common prefix or cycle backward."
      (interactive)
      (company-complete-common-or-cycle -1))
    (define-key company-active-map [return] 'company-complete-selection)
    (define-key company-active-map (kbd "RET") 'company-complete-selection)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<backtab>") 'company-complete-common-or-cycle-backward)
    (define-key company-active-map (kbd "S-TAB") 'company-complete-common-or-cycle-backward)
    (global-company-mode)))

(use-package company-statistics :ensure t
  :defer t
  :init
  (progn
    (setq company-statistics-file "~/.emacs.d/company-statistics-cache.el")
    (add-hook 'company-mode-hook 'company-statistics-mode)))

(use-package pos-tip :ensure t)
  
(use-package company-quickhelp :ensure t
  :commands company-quickhelp-manual-begin
  :init
  (defvar company-quickhelp-use-propertized-text t)
  (defvar company-quickhelp-color-foreground "#839496")
  (defvar company-quickhelp-color-background "#073642")
  (with-eval-after-load 'company
    (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))
    (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
    (company-quickhelp-mode 1)))

(use-package general :ensure t
  :config
  (general-define-key "M-W" 'toggle-frame-fullscreen))

(use-package editorconfig :ensure t
  :config
  (message "editorconfig started")
  (editorconfig-mode 1))

(use-package spacemacs-theme :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

(use-package helm :ensure t)
(use-package projectile :ensure t
  :config
  (message "projectile started")
  (projectile-mode))
(use-package helm-projectile :ensure t
  :config
  (message "helm-projectile started"))

(use-package spaceline :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package exec-path-from-shell :ensure t
  :config
  (message "exec-path-from-shell started")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "DEV_MODE")
    (exec-path-from-shell-initialize)))

(use-package expand-region :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

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
  (global-set-key "\M-k" 'fastnav-delete-char-forward)
  (global-set-key "\M-K" 'fastnav-delete-char-backward)
  (global-set-key "\M-m" 'fastnav-mark-to-char-forward)
  (global-set-key "\M-M" 'fastnav-mark-to-char-backward))

(use-package multiple-cursors :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package magit :ensure t)

(use-package flycheck :ensure t
  :config
  (global-flycheck-mode)
  (message "flycheck started"))

(use-package web-mode :ensure t
  :mode "\\.js[x]?\\'"
  :config
  (setq web-mode-content-types-alist
	'(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-expanding t))

(use-package emmet-mode :ensure t
  :init
  (add-hook 'web-mode-hook 'emmet-mode)
  :config
  (setq emmet-expand-jsx-className? t)
  (setq emmet-indentation 2))

(use-package yasnippet :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"
	  "~/.emacs.d/custom_snippets"))
  (yas-global-mode 1))

(use-package ggtags :ensure t
 :config
 (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
 (add-hook 'c-mode-common-hook (ggtags-mode 1)))

(use-package irony :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony :ensure t
  :init
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))

(use-package company-c-headers :ensure t
  :init
  (add-to-list 'company-backends 'company-c-headers))

(defun python-xpath (url)
  "Open a python shell, download the page from URL, parse etree."
  (interactive "sCount: ")
  (run-python)
  (python-shell-send-string (format "import requests; from lxml import etree; res = requests.get('%s'); root = etree.HTML(res.content)" url))
  (python-shell-switch-to-shell))

;; run as server?
(require 'server)
(unless (server-running-p) (server-start))

