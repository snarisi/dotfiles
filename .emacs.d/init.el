(setq load-prefer-newer t)

;; https://www.reddit.com/r/emacs/comments/3scsak/incredibly_slow_comint_eg_shell_compile_output_on/
(defun my-comint-shorten-long-lines (text)
  (let* ((regexp "\\(.\\{1000\\}[;,: ]\\)")
         (shortened-text (replace-regexp-in-string regexp "\\1\n" text)))
    (if (string= shortened-text text)
        text
      shortened-text)))
(add-hook 'comint-preoutput-filter-functions 'my-comint-shorten-long-lines)

(setq frame-title-format '("" "%b @ Emacs " emacs-version))

;; add directory for custom lisp
(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq ns-use-native-fullscreen nil)

;; make it real fullscreen
(setq frame-resize-pixelwise t)

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

;; print a default message in the empty scratch buffer opened at startup
(setq initial-scratch-message "*scratch*")

;; show matching parens
(show-paren-mode)

;; enable fullscreen
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)

(global-set-key (kbd "M-W") 'toggle-frame-fullscreen)

;; to get it to work in client mode
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '((font . "Ubuntu Mono")))

;; autoclose pairs
(electric-pair-mode 1)
(add-hook 'inferior-python-mode-hook
	  (lambda ()
	    (setq-local electric-pair-pairs '(append electric-pair-pairs (?\' . ?\')))))  ;; autoclose single quote

(add-hook 'shell-mode-hook
	  (lambda ()
	    (setq-local electric-pair-pairs '(append electric-pair-pairs (?\' . ?\')))))  ;; autoclose single quote

;; lets you navigate to error lines and maybe other stuff
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; lets you ask emacs what time it is
(defun show-current-time ()
  "Display a message showing the current time."
  (interactive)
  (message (format-time-string "%I:%M %p [%a %b %d, %Y]")))
(global-set-key (kbd "C-c DEL") 'show-current-time)

;; don't have to type "yes"
(fset 'yes-or-no-p 'y-or-n-p)

(defun disable-company-capf ()
  "Remove company-capf from company backends because it seems to mess up Python completion."
  (setq-local company-backends (remove 'company-capf company-backends)))
(add-hook 'python-mode-hook #'disable-company-capf)

;; set font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 170 :family "Ubuntu Mono" :weight light :width expanded))))
 '(lsp-ui-doc-background ((t (:background "#272A36"))))
 '(lsp-ui-sideline-global ((t (:background "selectedKnobColor")))))

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
 '(lsp-eldoc-render-all nil)
 '(lsp-enable-eldoc nil)
 '(lsp-ui-doc-enable t)
 '(lsp-ui-doc-position (quote top))
 '(lsp-ui-flycheck-enable nil)
 '(lsp-ui-peek-always-show nil)
 '(lsp-ui-peek-fontify (quote always))
 '(lsp-ui-sideline-delay 0.2)
 '(lsp-ui-sideline-enable t)
 '(lsp-ui-sideline-ignore-duplicate nil)
 '(lsp-ui-sideline-update-mode (quote line))
 '(package-selected-packages
   (quote
    (avy flycheck markdown-mode pyenv-mode helm-ag wgrep neotree diminish ggtags helm-gtags exec-path-from-shell omnisharp xref-js2 js2-refactor rjsx-mode python-pytest virtualenvwrapper yasnippet bash-completion helm-git-grep magit transpose-frame helm-smex helm-swoop helm-projectile multiple-cursors phi-search disable-mouse editorconfig expand-region evil doom-modeline lsp-ui company-lsp lsp-mode company-statistics company eldoc-eval shrink-path doom-themes use-package))))

(use-package doom-themes :ensure t
  :init
  (load-theme 'doom-one t)
  (defun load-theme-for-client (_)
    (load-theme 'doom-one t))
  (add-to-list 'after-make-frame-functions #'load-theme-for-client))

;; Themes
(use-package doom-modeline :ensure t
  :init (doom-modeline-init))

;; Misc
;; Make sure you have the right env vars
;; (use-package exec-path-from-shell :ensure t
;;   :config
;;   (exec-path-from-shell-initialize)
;;   (exec-path-from-shell-copy-env "PYENV_ROOT")
;;   (exec-path-from-shell-copy-env "DEV_MODE"))

(use-package diminish :ensure t
  :config
  (diminish 'disable-mouse-global-mode))

(use-package evil :ensure t
  :config
  (setq evil-normal-state-cursor '("#51afef" box))
  (setq evil-insert-state-cursor '("#98be65" box))

  ;; I want "insert" state to act like normal emacs. Maybe a bad way to do it.
  (setq evil-insert-state-map evil-emacs-state-map)
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-force-normal-state))

(use-package expand-region :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package editorconfig :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package disable-mouse :ensure t
  :diminish disable-mouse-global-mode
  :init
  (global-disable-mouse-mode))

(use-package phi-search :ensure t
  :config
  (global-set-key (kbd "C-s") 'phi-search)
  (global-set-key (kbd "C-r") 'phi-search-backward))

(use-package multiple-cursors :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package avy :ensure t
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-c a c") 'avy-goto-char)
  (global-set-key (kbd "C-c a a") 'avy-goto-char-timer)
  (global-set-key (kbd "C-c a w") 'avy-goto-word-0)
  (global-set-key (kbd "C-c a l") 'avy-goto-line)
  (global-set-key (kbd "C-c a m") 'avy-move-line)
  (global-set-key (kbd "C-c a k") 'avy-kill-whole-line)
  (global-set-key (kbd "C-c C-j") 'avy-resume))

(use-package winner-mode-enable
  :init
  (winner-mode t))

(use-package undo-tree :ensure t
  :diminish 'undo-tree-mode
  :init
  (global-undo-tree-mode)
  (global-set-key (kbd "C-c u") 'undo-tree-visualize))

(use-package projectile :ensure t
  :diminish 'projectile-mode
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :config
  (defun my-projectile-project-switch-action ()
    (when
	(and
	 (projectile-project-name)
	 (venv-is-valid (projectile-project-name)))
      (message "now working on: %s" (projectile-project-name))
      (venv-workon (projectile-project-name)))
    (helm-projectile-find-file))
  ;; (setq projectile-switch-project-action #'my-projectile-project-switch-action)
  )

(use-package helm-projectile :ensure t
  :init
  (helm-projectile-on)
  (global-set-key (kbd "C-c g") 'helm-projectile-ag))

(use-package helm :ensure t
  :config
  (setq helm-echo-input-in-header-line t)
  (defun helm-hide-minibuffer-maybe ()
    "Make helm use the header line as input instead of the minibuffer."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
	(overlay-put ov 'window (selected-window))
	(overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
				`(:background ,bg-color :foreground ,bg-color)))
	(setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  (setq helm-split-window-inside-p t))

(use-package helm-swoop :ensure t
  :config
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-use-fuzzy-match t)
  (global-set-key (kbd "C-c o") 'helm-swoop)
  (global-set-key (kbd "C-S-c o") 'helm-multi-swoop))

(use-package helm-smex :ensure t
  :config
  (global-set-key [remap execute-extended-command] #'helm-smex)
  (global-set-key (kbd "M-X") #'helm-smex-major-mode-commands))

(use-package shell
  :config
  (global-set-key (kbd "C-c s") 'shell)
  (define-key shell-mode-map (kbd "M-r") 'helm-comint-input-ring)
  (define-key shell-mode-map (kbd "M-?") nil))

(use-package transpose-frame :ensure t
  :config
  (global-set-key (kbd "C-c f t") 'transpose-frame)
  (global-set-key (kbd "C-c f f") 'flip-frame)
  (global-set-key (kbd "C-c f l") 'flop-frame))

(use-package magit :ensure t
  :init
  (global-set-key (kbd "C-c m") 'magit-status))

(use-package helm-git-grep :ensure t
  :config
  ;; (global-set-key (kbd "C-c g") 'helm-git-grep)
  ;; Invoke `helm-git-grep' from isearch.
  ;; (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
  ;; Invoke `helm-git-grep' from other helm.
  ;; (eval-after-load 'helm
  ;;   '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))
  )

(use-package wgrep :ensure t)

(use-package neotree :ensure t
  :init
  (global-set-key (kbd "C-c \\") 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Markdown
(use-package markdown-mode :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Auto-Complete
(use-package company :ensure t
  :diminish company-mode
  :init
  ;; (progn
  ;;   (setq company-idle-delay 0
  ;;         company-minimum-prefix-length 2
  ;;         company-require-match nil
  ;;         company-dabbrev-ignorecom-case nil
  ;;         company-dabbrev-downcase nil))
  (progn
    (setq company-idle-delay 0.5
          company-minimum-prefix-length 2
          company-require-match nil))
  :config
  (global-company-mode)
  (global-set-key (kbd "C-c c") 'company-complete))

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
	'("~/.emacs.d/snippets/snippets"
	  "~/.emacs.d/custom_snippets"))
  (yas-global-mode 1))

;; Python
(use-package flycheck :ensure t
  :config
  (setq flycheck-python-flake8-executable "flake8")
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (global-flycheck-mode))

(use-package virtualenvwrapper :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package pyenv-mode :ensure t)

(use-package python
  :init
  (defun chdir-to-project-root ()
    "When opening a python shell while a projectile project is active, changes directory of python shell to project root"
    (when (projectile-project-name)
      (python-shell-send-string "import os")
      (python-shell-send-string (format "os.chdir('%s')" (projectile-project-root)))))
  (add-hook 'python-shell-first-prompt-hook #'chdir-to-project-root))


(use-package python-pytest :ensure t
  :init
  (define-key python-mode-map (kbd "C-c p ,") 'python-pytest-popup))

;; Javascript
(use-package rjsx-mode :ensure t
  :init
  (defun set-jsx-indentation ()
    (setq-local sgml-basic-offset 2))
  (add-hook 'js-jsx-mode-hook #'set-jsx-indentation)
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
  (define-key js2-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key js2-mode-map (kbd "M-,") 'xref-pop-marker-stack)
  (define-key js2-mode-map (kbd "M-?") 'xref-find-references))

;; C#
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
    (define-key omnisharp-mode-map (kbd "M-?") 'omnisharp-helm-find-usages)
    (define-key omnisharp-mode-map (kbd "C-c i") 'helm-imenu))
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
  (eval-after-load 'company '(add-to-list 'company-backends 'company-omnisharp)))

;; gtags
(use-package helm-gtags :ensure t
  :diminish
  :init
  (setq helm-gtags-prefix-key (kbd "C-c t")
	helm-gtags-suggested-key-mapping t)
  (define-key helm-gtags-mode-map (kbd "M-.") nil) ;; keep the xref keybinding for this
  :config
  ;; (add-hook 'prog-mode-hook 'helm-gtags-mode)
  (helm-gtags-mode))

;; Language Server Protocol
(use-package lsp-mode :ensure t
  :diminish 'lsp-mode
  :config
  ;; (setq lsp-highlight-symbol-at-point nil)  ;; seems to slow everything down?
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  ;; This is copied from https://github.com/emacs-lsp/lsp-python/blob/master/lsp-python.el
  ;; but I changed it to use /usr/local/bin/pyls because it stopped working with pyenv
  (lsp-define-stdio-client lsp-python "python"
			   (lsp-make-traverser #'(lambda (dir)
						   (directory-files
						    dir
						    nil
						    "setup.py")))
			   '("pyls"))
  (add-hook 'python-mode-hook #'lsp-python-enable)

  (use-package company-lsp :ensure t
    :config
    (push 'company-lsp company-backends))

  (use-package lsp-ui :ensure t
    :preface
    (setq lsp-ui-sideline-enable nil
	  lsp-ui-flycheck-enable nil
	  lsp-highlight-symbol-at-point t
	  lsp-ui-doc-position (quote top))
    :config
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map (kbd "C-c i") 'helm-imenu)
    (define-key helm-gtags-mode-map (kbd "M-.") nil) ;; keep the xref keybinding for this
    (define-key lsp-ui-mode-map (kbd "C-c l r") 'lsp-restart-workspace)))
