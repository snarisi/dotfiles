;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; just use normal emacs keys for insert state and emacs state
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "<RET>") 'newline-and-indent)

(setq evil-emacs-state-map (make-sparse-keymap))
(define-key evil-emacs-state-map (kbd "C-z") 'evil-exit-emacs-state)
(define-key evil-emacs-state-map (kbd "<RET>") 'newline-and-indent)

(setq
 doom-font (font-spec :family "Ubuntu Mono" :size 17)
 doom-big-font (font-spec :family "Ubuntu Mono" :size 19)
 doom-theme 'doom-tomorrow-night)

(setq-default
 truncate-lines nil
 word-wrap nil)

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

;; I think this prevents a thing where emacs freezes, I forget
(define-key shell-mode-map (kbd "M-?") nil)

;; make _ part of the word in python-mode
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; make - part of the word in elisp-mode
(add-hook 'emacs-lisp-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))

(add-hook 'python-mode-hook #'(lambda ()
                                (when (executable-find "ipython")
                                  (setq python-shell-interpreter "ipython"
                                        python-shell-interpreter-args "-i --simple-prompt --no-color-info"
                                        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                                        python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
                                        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                                        python-shell-completion-setup-code
                                        "from IPython.core.completerlib import module_completion"
                                        python-shell-completion-string-code
                                        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))))

(cond (IS-MAC
       (setq mac-option-modifier 'meta
             mac-command-modifier 'super)
       (add-hook 'doom-init-ui-hook #'ns-auto-titlebar-mode)))

;; mostly because lsp-mode spams warnings
;; but do either of these work?
(setq-default display-warning-minimum-level :error)
(setq display-warning-minimum-level :error)


(set-popup-rule! "*shell*" :quit nil)
(set-popup-rule! "*Python*" :quit nil)
(setq helm-split-window-inside-p t)

;; lets you navigate to error lines and maybe other stuff
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; use straight.el to manage some packages
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; from https://git.sr.ht/%7Ekristjansson/lsp-python-ms/tree/master/lsp-python-ms.el
(use-package lsp-mode
  :straight t
  :hook (lsp-after-open-hook . lsp-enable-imenu)
  :config
  (setq lsp-print-io t)
  (defvar lsp-python-ms-dir nil
    "Path to langeuage server directory containing Microsoft.Python.LanguageServer.dll")

  (defvar lsp-python-ms-dotnet nil
    "Path to dotnet executable.")

  (setq lsp-python-ms-dir (expand-file-name "~/build/python-language-server/output/bin/Release/"))

  (defun lsp-python-ms--get-python-ver-and-syspath ()
    "return list with pyver-string and json-encoded list of python search paths."
    (let ((python (executable-find python-shell-interpreter))
          (init "from __future__ import print_function; import sys; import json;")
          (ver "print(\"%s.%s\" % (sys.version_info[0], sys.version_info[1]));")
          (sp (concat "print(json.dumps(sys.path))")))
      (with-temp-buffer
        (call-process python nil t nil "-c" (concat init ver sp))
        (subseq (split-string (buffer-string) "\n") 0 2))))

  (defun lsp-python-ms--extra-init-params ()
    (destructuring-bind (pyver pysyspath)
        (lsp-python-ms--get-python-ver-and-syspath)
      `(:interpreter
        (:properties (
                      :InterpreterPath ,(executable-find python-shell-interpreter)
                      ;; this database dir will be created if required
                      :DatabasePath ,(expand-file-name (concat lsp-python-ms-dir "db/"))
                      :Version ,pyver))
        ;; preferredFormat "markdown" or "plaintext"
        ;; experiment to find what works best -- over here mostly plaintext
        :displayOptions (
                         :preferredFormat "plaintext"
                         :trimDocumentationLines :json-false
                         :maxDocumentationLineLength 0
                         :trimDocumentationText :json-false
                         :maxDocumentationTextLength 0)
        :searchPaths ,(json-read-from-string pysyspath))))

  (defun lsp-python-ms--workspace-root ()
    "Get the root, or just return `default-directory'."
    (let ((proj (projectile-project-root)))
      (if proj proj default-directory)))

  (defun lsp-python-ms--find-dotnet ()
    "Get the path to dotnet, or return `lsp-python-ms-dotnet'."
    (let ((dotnet (executable-find "dotnet")))
      (if dotnet dotnet lsp-python-ms-dotnet)))

  (defun lsp-python-ms--filter-nbsp (kind str)
    "Filter nbsp entities from STR."
    (replace-regexp-in-string "&nbsp;" " " str))

  (defun filter-lsp--render-string (str)
    (replace-regexp-in-string "&nbsp;" " " str))

  (setq lsp-render-markdown-markup-content #'lsp-python-ms--filter-nbsp)
  (advice-add 'lsp-ui-doc--extract
              :filter-return #'lsp-python-ms--filter-nbsp)
  (advice-add 'lsp--render-string
              :filter-return #'filter-lsp--render-string)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     `(,(lsp-python-ms--find-dotnet) ,(concat lsp-python-ms-dir "Microsoft.Python.LanguageServer.dll")) )
                    :major-modes '(python-mode)
                    :server-id 'pyls
                    :initialization-options #'lsp-python-ms--extra-init-params))
  (add-hook 'python-mode-hook 'lsp))


;; (def-package! lsp-mode
;;   :hook (lsp-after-open-hook . lsp-enable-imenu)
;;   :config
;;   (setq lsp-message-project-root-warning t))

;; (def-package! lsp-python
;;   :after (lsp-mode)
;;   :init (add-hook 'python-mode-hook #'lsp-python-enable))

(def-package! eglot
  :config
  (defvar lsp-python-ms-dir nil
    "Path to langeuage server directory containing Microsoft.Python.LanguageServer.dll")

  (defvar lsp-python-ms-dotnet nil
    "Path to dotnet executable.")

  (setq lsp-python-ms-dir (expand-file-name "~/build/python-language-server/output/bin/Release/"))

  (defun lsp-python-ms--get-python-ver-and-syspath ()
    "return list with pyver-string and json-encoded list of python search paths."
    (let ((python (executable-find python-shell-interpreter))
          (init "from __future__ import print_function; import sys; import json;")
          (ver "print(\"%s.%s\" % (sys.version_info[0], sys.version_info[1]));")
          (sp (concat "print(json.dumps(sys.path))")))
      (with-temp-buffer
        (call-process python nil t nil "-c" (concat init ver sp))
        (subseq (split-string (buffer-string) "\n") 0 2))))

  (defclass eglot-ms-pyls (eglot-lsp-server) ()
    :documentation "Microsoft's Python Language Server")

  (cl-defmethod eglot-initialization-options ((server eglot-ms-pyls))
    (destructuring-bind (pyver pysyspath)
        (lsp-python-ms--get-python-ver-and-syspath)
      `(:interpreter
        (:properties (
                      :InterpreterPath ,(executable-find python-shell-interpreter)
                      ;; this database dir will be created if required
                      :DatabasePath ,(expand-file-name (concat lsp-python-ms-dir "db/"))
                      :Version ,pyver))
        ;; preferredFormat "markdown" or "plaintext"
        ;; experiment to find what works best -- over here mostly plaintext
        :displayOptions (
                         :preferredFormat "plaintext"
                         :trimDocumentationLines :json-false
                         :maxDocumentationLineLength 0
                         :trimDocumentationText :json-false
                         :maxDocumentationTextLength 0)
        :searchPaths ,(json-read-from-string pysyspath))))

  (defun eglot--ms-pyls-contact ()
    (cons 'eglot-ms-pyls
          (list (lsp-python-ms--find-dotnet) (concat lsp-python-ms-dir "Microsoft.Python.LanguageServer.dll"))))

  (add-to-list 'eglot-server-programs '(python-mode . (eglot-ms-pyls (lsp-python-ms--find-dotnet) (concat lsp-python-ms-dir "Microsoft.Python.LanguageServer.dll"))))
  ;; (add-hook 'python-mode-hook 'eglot-ensure)
  )


(def-package! company-lsp
  :straight t
  :config
  (setq company-lsp-enable-recompletion t)
  (set-company-backend! 'lsp-mode 'company-lsp))

(def-package! lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (set-lookup-handlers! 'lsp-ui-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 50
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-flycheck t
        lsp-highlight-symbol-at-point nil
        lsp-ui-flycheck-enable t))

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
(def-package! dockerfile-mode)
(def-package! docker)

(set-company-backend! 'shell-mode 'company-capf)
(set-company-backend! 'inferior-python-mode 'company-capf)

;; (def-package! company-box
;;   :hook (company-mode . company-box-mode))

;; (after! flycheck
;;   (setq flycheck-python-flake8-executable "flake8"
;;         flycheck-display-errors-delay 0.5))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil))

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


;; lets you ask emacs what time it is
(defun show-current-time ()
  "Display a message showing the current time."
  (interactive)
  (message (format-time-string "%I:%M %p [%a %b %d, %Y]")))

;;;autoload
(defun +repl/open (arg)
  "Open a repl buffer in the current window."
  (interactive "P")
  (+eval/open-repl t))

;;;###autoload
(defun +repl/open-popup (arg)
  "Open a repl popup window."
  (interactive "P")
  (+eval/open-repl nil))

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

(defun open-python-repl-in-project-root (orig-fun &rest args)
  (let ((default-directory (doom-project-root)))
    (apply orig-fun args)))

(advice-add '+python/repl :around #'open-python-repl-in-project-root)

(map!
 (:leader
   :desc "Easymotion next"  :n "j"  #'evilem-motion-next-line
   :desc "Easymotion prev"  :n "k"  #'evilem-motion-previous-line

   (:desc "code" :prefix "c"
     :desc "Pop marker stack"      :n ","  #'xref-pop-marker-stack
     (:after pyvenv
       :desc "Change virtualenv"     :n "v"  #'pyvenv-workon)

     ;; Can't get set-lookup-handlers to work
     (:after eglot
       (:map eglot-mode-map
         :desc "Documentation at point"          :n "/"  #'eglot-help-at-point
         :desc "Jump to definition"              :n "d"  #'xref-find-definitions
         :desc "Jump to references"              :n "D"  #'xref-find-references))

     (:after lsp-ui
       :desc "Highlight symbol at point"  :n "h"  #'lsp-symbol-highlight
       :desc "Documentation at point"     :n "h"  #'lsp-describe-thing-at-point
       ;; don't know why this stopped working
       (:map lsp-ui-mode-map
         :desc "Jump to definition"         :n "d"  #'xref-find-definitions
         :desc "Jump to references"         :n "D"  #'lsp-ui-peek-find-references)))

   ;; swap doom defaults
   (:desc "buffer" :prefix "b"
     :desc "Switch buffer"            :n "b"  #'ivy-switch-buffer
     :desc "Switch workspace buffer"  :n "B"  #'+ivy/switch-workspace-buffer)

   (:desc "open" :prefix "o"
     :desc "REPL"              :n "r"  #'+repl/open
     :desc "REPL in popup"     :n "R"  #'+repl/open-popup
     :desc "Shell"              :n "s"  #'+shell/open
     :desc "Shell in popup"     :n "S"  #'+shell/open-popup)

   (:desc "window" :prefix "w"
     :desc "other-window"      :n "o"  #'ace-window)

   (:desc "toggle" :prefix "t"
     :desc "Show current time" :n "t"  #'show-current-time))

 ;; company completion
 :i "C-c c"    #'+company/complete

 (:after multiple-cursors
   (:map evil-emacs-state-map
     "C-S-c C-S-c" #'mc/edit-lines
     "C->"         #'mc/mark-next-like-this
     "C-<"         #'mc/mark-previous-like-this
     "C-c C-<"     #'mc/mark-all-like-this))

 (:after ivy
   (:map ivy-minibuffer-map
     "C-w" #'ivy-yank-word))

 )
;; Don't why it doesn't work when the bindings are in another file
;; (load! "+bindings")
