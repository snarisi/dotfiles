;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Ubuntu Mono" :size 17))
(setq doom-big-font (font-spec :family "Ubuntu Mono" :size 19))
(setq doom-theme 'doom-tomorrow-night)

(setq-default truncate-lines nil)

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)
(after! solaire-mode
  (solaire-mode-swap-bg))

;; make _ part of the word in python-mode
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; make - part of the word in elisp-mode
(add-hook 'emacs-lisp-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))

(cond (IS-MAC
       (setq mac-option-modifier 'meta
             mac-command-modifier 'super)))

;; (def-package! lsp-mode
;;   :hook (lsp-after-open-hook . lsp-enable-imenu)
;;   :config
;;   (lsp-define-stdio-client lsp-python "python"
;; 			   (lsp-make-traverser #'(lambda (dir)
;; 						   (directory-files
;; 						    dir
;; 						    nil
;; 						    "setup.py")))
;; 			   '("pyls"))
;;   (add-hook 'python-mode-hook #'lsp-python-enable))
;;
(def-package! lsp-mode
  :hook (lsp-after-open-hook . lsp-enable-imenu))

(def-package! lsp-python
  :after (lsp-mode)
  :init (add-hook 'python-mode-hook #'lsp-python-enable))

(def-package! company-lsp
  :config
  (setq company-lsp-enable-recompletion t)
  (set-company-backend! 'lsp-mode 'company-lsp))

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (set-lookup-handlers! 'lsp-ui-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 50
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-flycheck nil
        lsp-ui-flycheck-enable nil))


(def-package! virtualenvwrapper
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(def-package! pyvenv
  :config
  (set-env! "WORKON_HOME"))

(def-package! bash-completion
  :init
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
	    'bash-completion-dynamic-complete))

(def-package! multiple-cursors)

(set-company-backend! 'shell-mode 'company-capf)
(set-company-backend! 'inferior-python-mode 'company-capf)

;; (def-package! company-box
;;   :hook (company-mode . company-box-mode))

(after! flycheck
  (setq flycheck-python-flake8-executable "flake8"
        flycheck-display-errors-delay 0.5))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2))

(after! csharp-mode
  (defun my-csharp-mode-hook ()
    (add-to-list 'c-default-style '(csharp-mode . "bsd"))
    (setq c-basic-offset 4)
    (setq tab-width 4)
    (setq evil-shift-width 4))
  (add-hook 'csharp-mode-hook #'my-csharp-mode-hook))

;; (after! omnisharp
;;   (defun my-csharp-mode-setup ()
;;     (require 'omnisharp-settings)
;;     (require 'omnisharp-utils)
;;     (setq indent-tabs-mode nil)
;;     (setq c-syntactic-indentation t)
;;     (c-set-style "ellemtel")
;;     (setq c-basic-offset 4)
;;     (setq tab-width 4)
;;     (setq evil-shift-width 4))
;;   (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t))


;;;###autoload
(defun +shell/open (arg)
  "Open a shell buffer in the current window. If ARG (universal argument) is
non-nil, cd into the current project's root."
  (interactive "P")
  (let ((default-directory
          (if arg
              (doom-project-root 'nocache)
            default-directory)))
    ;; Doom's switch-buffer hooks prevent themselves from triggering when
    ;; switching from buffer A back to A. Because `multi-term' uses `set-buffer'
    ;; before `switch-to-buffer', the hooks don't trigger, so we use this
    ;; roundabout way to trigger them properly.
    (switch-to-buffer (save-window-excursion (shell)))))

;;;###autoload
(defun +shell/open-popup (arg)
  "Open a terminal popup window. If ARG (universal argument) is
non-nil, cd into the current project's root."
  (interactive "P")
  (let ((default-directory
          (if arg
              (doom-project-root 'nocache)
            default-directory)))
    (pop-to-buffer (save-window-excursion (shell)))))

(map!
 (:leader
   :desc "Easymotion next"  :n "j"  #'evilem-motion-next-line
   :desc "Easymotion prev"  :n "k"  #'evilem-motion-previous-line

   (:desc "search" :prefix "/"
     :desc "Counsel-ag"            :nv "g" #'counsel-ag)

   (:desc "code" :prefix "c"
     :desc "Pop marker stack"      :n ","  #'xref-pop-marker-stack
     ;; this should be :after venv-mode or whatever
     (:after virtualenvwrapper
       :desc "Change virtualenv"     :n "v"  #'venv-workon))

   (:desc "open" :prefix "o"
     :desc "Shell"              :n "s"  #'+shell/open
     :desc "Shell in popup"     :n "S"  #'+shell/open-popup))

 ;; company completion
 :i "C-c c"    #'+company/complete

 (:after multiple-cursors
   (:map evil-emacs-state-map
     "C-S-c C-S-c" #'mc/edit-lines
     "C->"         #'mc/mark-next-like-this
     "C-<"         #'mc/mark-previous-like-this
     "C-c C-<"     #'mc/mark-all-like-this))

 ;; re-add default emacs bindings as I find things that are annoying
 (:after evil
   (:map evil-insert-state-map
     "C-n" #'next-line
     "C-p" #'previous-line
     "C-o" #'open-line
     "C-b" #'backward-char
     "C-f" #'forward-char
     "M-b" #'backward-word
     "M-f" #'forward-word
     "M-d" #'kill-word
     "C-w" #'kill-region
     "M-w" #'ns-copy-including-secondary
     "C-SPC" #'set-mark-command
     "C-d"   #'delete-char
     "<M-backspace>" #'backward-kill-word))
 )

;; Don't why it doesn't work when the bindings are in another file
;; (load! "+bindings")
