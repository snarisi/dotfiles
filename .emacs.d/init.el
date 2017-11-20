;; add directory for custom lisp
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/anaconda-mode")

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
(setq python-shell-interpreter "/usr/local/bin/ipython"
      python-shell-interpreter-args "-i")
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")

;; 4 space tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; use bash for ansi-term -- doesn't work?
(setq explicit-shell-file-name "/bin/bash")
(setenv "SHELL" "/bin/bash")

;; use C-c C-y to yank while in ansi-term
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-y") 'term-paste))

;; try to fix key binding for term
;; (eval-after-load "term"
;;   '(define-key term-raw-map (kbd "C-x C-j") 'term-line-mode))

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
    (helm-smex helm-projectile-ag helm-git-grep ox-gfm markdown-mode swoop helm-ag flycheck-irony multi-term company-irony-c-headers cmake-ide rainbow-delimiters company-irony company-c-headers pdf-tools irony speed-type company-anaconda company-quickhelp ac-anaconda anaconda-mode auto-complete-c-headers auto-complete-clang-async projectile-speebar ggtags helm-gtags jedi auto-complete company-jedi ycmd epc web-mode company-ycmd company redis pyvenv ycmd-eldoc company-elisp helm-rtags rtags-helm rtags yasnippet multiple-cursors fastnav emmet-mode expand-region leuven-theme hydandata-light-theme flatui-theme flycheck exec-path-from-shell virtualenvwrapper spaceline magit helm-projectile projectile helm editorconfig spacemacs-theme doom-themes avy general use-package)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#839496")
 '(pos-tip-use-relative-coordinates nil)
 '(safe-local-variable-values
   (quote
    ((eval setq company-clang-arguments
           (list "-isystem /usr/include/qt" "-isystem /usr/include/qt/QtWidgets" "-isystem /usr/include/qt/QtGui" "-isystem /usr/include/qt/QtCore" "-isystem /usr/include/libdrm" "-I/usr/lib/qt/mkspecs/linux-g++" "std=c++11" "-fPIC")
           flycheck-clang-include-path
           (list "/usr/include/qt/QtWidgets" "/usr/include/qt/QtCore" "/usr/lib/qt/mkspecs/linux-g++" "/usr/include/qt/QtGui" "/usr/include/qt")
           flycheck-clang-args
           (list "-std=c++11" "-fPIC")
           cmake-ide-build-dir "/home/ringo/projects/cpp/book/ch03/qapp")
     (eval setq company-clang-arguments
           (list "-isystem /usr/include/qt" "-isystem /usr/include/qt/QtWidgets" "-isystem /usr/include/qt/QtGui" "-isystem /usr/include/qt/QtCore" "-isystem /usr/include/libdrm" "-I/usr/lib/qt/mkspecs/linux-g++" "std=c++11" "-fPIC")
           flycheck-clang-include-path
           (list "/usr/include/qt/QtWidgets" "/usr/include/qt/QtCore" "/usr/lib/qt/mkspecs/linux-g++" "/usr/include/qt/QtGui" "/usr/include/qt")
           flycheck-clang-args
           (list "-std=c++11" "-fPIC")
           irony-additional-clang-options
           (list "-isystem /usr/include/qt" "-isystem /usr/include/qt/QtWidgets" "-isystem /usr/include/qt/QtGui" "-isystem /usr/include/qt/QtCore" "-isystem /usr/include/libdrm" "-I/usr/lib/qt/mkspecs/linux-g++" "std=c++11" "-fPIC"))
     (eval setq company-clang-arguments
           (list "-isystem /usr/include/qt" "-isystem /usr/include/qt/QtWidgets" "-isystem /usr/include/qt/QtGui" "-isystem /usr/include/qt/QtCore" "-isystem /usr/include/libdrm" "-I/usr/lib/qt/mkspecs/linux-g++" "std=c++11" "-fPIC")
           flycheck-clang-include-path
           (list "/usr/include/qt/QtWidgets" "/usr/include/qt/QtCore" "/usr/lib/qt/mkspecs/linux-g++" "/usr/include/qt/QtGui" "/usr/include/qt")
           flycheck-clang-args
           (list "-std=c++11" "-fPIC")))))
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

;; (require 'anaconda-mode)

(use-package anaconda-mode :ensure t
  :defer t
  :config
  ;; (local-set-key (kbd "M-.") 'anaconda-mode-find-definitions)
  ;; (local-set-key (kbd "M-,") 'anaconda-mode-go-back)
  ;; (local-set-key (kbd "M-?") 'anaconda-mode-show-doc)
  ;; (local-set-key (kbd "M-/") 'anaconda-mode-find-assignments)
  ;; (local-set-key (kbd "M-r") 'anaconda-mode-find-references)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda :ensure t
  :init
  ;; for some reason quickhelp doesn't work unless this is here
  ;; (defun my-annotation-function (candidate)
  ;;   (let ((description (get-text-property 0 'description candidate)))
  ;;     (when description
  ;;       (concat "<" description ">"))))
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

(require 'company-anaconda)
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

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
    (setq company-idle-delay 0.1
          company-minimum-prefix-length 2
          company-require-match nil
          company-dabbrev-ignorecom-case nil
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
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
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
  (general-define-key "M-W" 'toggle-frame-fullscreen)
  (defun open-term-bash ()
    "Opens a term buffer and runs bash"
    (interactive)
    (ansi-term "/bin/bash")
    (let ((new-buffer-name (generate-new-buffer-name "term")))
      (rename-buffer new-buffer-name)
      (when (projectile-project-name)
        (comint-send-string new-buffer-name (format "workon %s\n" (projectile-project-name))))))

  (defun open-term-redis ()
    "Opens a term buffer and launches redis-cli."
    (interactive)
    (ansi-term "~/redis_open.py")
    (let ((new-buffer-name (generate-new-buffer-name "redis")))
      (rename-buffer new-buffer-name)))
  (general-define-key "C-c t" 'open-term-bash)
  (general-define-key "C-c r" 'open-term-redis)
  (general-define-key "C-c s" 'shell))

(use-package editorconfig :ensure t
  :config
  (message "editorconfig started")
  (editorconfig-mode 1))

(use-package spacemacs-theme :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t)
  (defun load-theme-for-client (_)
    (load-theme 'spacemacs-dark t))
  (add-to-list 'after-make-frame-functions #'load-theme-for-client))

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

(use-package magit :ensure t
  :config
  (defun my-post-push-hook (orig-fun &rest args)
    "Opens buildcentral after pushing code to a yipit branch."
    (when (and (projectile-project-name) (string-match-p "Yipit" (magit-get "remote" "origin" "url")))
      (shell-command-to-string (format "open -g https://builds.yipit.systems/%s" (projectile-project-name)))))
  (advice-add 'magit-push :around #'my-post-push-hook))

(use-package flycheck :ensure t
  :config
  (global-flycheck-mode)
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

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

;; (use-package company-irony :ensure t
;;   :init
;;   (eval-after-load 'company
;;     '(add-to-list
;;       'company-backends 'company-irony)))

(use-package company-irony :ensure t)

(use-package company-irony-c-headers :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list
      'company-backends '(company-irony-c-headers company-irony))))

(use-package flycheck-irony :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; (use-package company-c-headers :ensure t
;;   :init
;;   (add-to-list 'company-backends 'company-c-headers)
;;   :config
;;   (add-to-list 'company-c-headers-path-system "/usr/include/qt"))

(use-package cmake-ide :ensure t
  :config
  (cmake-ide-setup))

(use-package multi-term :ensure t
  :config
  (setq multi-term-program "/bin/bash")
  (setq term-bind-key-alist
        '(("C-c C-c" . term-interrupt-subjob)
          ("C-c C-e" . term-send-esc)
          ("C-c C-j" . term-line-mode)
          ;; ("C-p" . previous-line)
          ;; ("C-n" . next-line)
          ("C-s" . isearch-forward)
          ("C-r" . isearch-backward)
          ("C-m" . term-send-return)
          ("C-y" . term-paste)
          ("M-f" . term-send-forward-word)
          ("M-b" . term-send-backward-word)
          ("M-o" . term-send-backspace)
          ("M-p" . term-send-up)
          ("M-n" . term-send-down)
          ("M-M" . term-send-forward-kill-word)
          ("M-N" . term-send-backward-kill-word)
          ("<C-backspace>" . term-send-backward-kill-word)
          ("M-r" . term-send-reverse-search-history)
          ("M-d" . term-send-delete-word)
          ("M-DEL" . term-send-backward-kill-word)
          ("M-," . term-send-raw)
          ("M-." . comint-dynamic-complete)
          ("M-[" . multi-term-prev)
          ("M-]" . multi-term-next))))

(use-package org :ensure t
  :config
  (add-to-list 'org-export-backends 'md))

(use-package markdown-mode :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package ox-gfm :ensure t
  :defer t
  :init
  (eval-after-load "org"
  '(require 'ox-gfm nil t)))

(use-package swoop :ensure t
  :config
  (setq swoop-font-size-change: nil)
  (global-set-key (kbd "C-c o")   'swoop)
  (global-set-key (kbd "C-S-c o") 'swoop-multi)
  (global-set-key (kbd "C-M-s")   'swoop-pcre-regexp)
  (global-set-key (kbd "C-S-o") 'swoop-back-to-last-position))

(use-package helm-projectile-ag
  :init
  (global-set-key (kbd "C-c g") 'helm-projectile-ag))

(use-package helm-git-grep :ensure t
  :config
  ;; (global-set-key (kbd "C-c g") 'helm-git-grep)
  ;; Invoke `helm-git-grep' from isearch.
  (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
  ;; Invoke `helm-git-grep' from other helm.
  (eval-after-load 'helm
    '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm)))

(use-package helm-smex :ensure t
  :config
  (global-set-key [remap execute-extended-command] #'helm-smex)
  (global-set-key (kbd "M-X") #'helm-smex-major-mode-commands))

(defun python-xpath (url)
  "Open a python shell, download the page from URL, parse etree."
  (interactive "sURL: ")
  (run-python)
  (python-shell-send-string (format "import requests; from lxml import etree; res = requests.get('%s'); root = etree.HTML(res.content)" url))
  (python-shell-switch-to-shell))

(defun redis-cli (redis-uri)
  (interactive "sREDIS_URI: ")
  (if redis-uri
      (ansi-term (format "REDIS_URI=%s ~/open_redis.py" redis-uri)))
  (ansi-term "~/open_redis.py"))

(defun ipdb-set-trace ()
  (interactive)
  (insert-string "import ipdb; ipdb.set_trace()"))

(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c d") 'ipdb-set-trace))

;; run as server?
(require 'server)
(unless (server-running-p) (server-start))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
