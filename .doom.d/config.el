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

(setq-default display-warning-minimum-level :error)

(set-popup-rule! "*shell*" :quit nil)
(set-popup-rule! "*Python*" :quit nil)
(setq helm-split-window-inside-p t)

;; lets you navigate to error lines and maybe other stuff
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; mostly copied from https://vxlabs.com/2018/11/19/configuring-emacs-lsp-mode-and-microsofts-visual-studio-code-python-language-server/
(def-package! lsp-mode
  :hook (lsp-after-open-hook . lsp-enable-imenu)
  :config
  (setq lsp-print-io nil)
  (setq lsp-message-project-root-warning t)

  ;; mostly because lsp-mode spams warnings
  (setq display-warning-minimum-level :error)

  (setq ms-pyls-dir (expand-file-name "~/build/python-language-server/output/bin/Release/"))

  ;; this gets called when we do lsp-describe-thing-at-point in lsp-methods.el
  ;; we remove all of the "&nbsp;" entities that MS PYLS adds
  ;; this is mostly harmless for other language servers
  (defun render-markup-content (kind content)
    (message kind)
    (replace-regexp-in-string "&nbsp;" " " content))
  (setq lsp-render-markdown-markup-content #'render-markup-content)

  ;; it's crucial that we send the correct Python version to MS PYLS, else it returns no docs in many cases
  ;; furthermore, we send the current Python's (can be virtualenv) sys.path as searchPaths
  (defun get-python-ver-and-syspath (workspace-root)
    "return list with pyver-string and json-encoded list of python search paths."
    (let ((python (executable-find python-shell-interpreter))
          (ver "import sys; print(\"{}.{}\".format(sys.version_info[0], sys.version_info[1]));")
          (sp (concat "import json; sys.path.insert(0, '" workspace-root "'); print(json.dumps(sys.path))")))
      (with-temp-buffer
        (call-process python nil t nil "-c" (concat ver sp))
        (subseq (split-string (buffer-string) "\n") 0 2))))

  ;; I based most of this on the vs.code implementation:
  ;; https://github.com/Microsoft/vscode-python/blob/master/src/client/activation/languageServer/languageServer.ts#L219
  ;; (it still took quite a while to get right, but here we are!)
  (defun ms-pyls-extra-init-params (workspace)
    (destructuring-bind (pyver pysyspath) (get-python-ver-and-syspath (lsp--workspace-root workspace))
      `(:interpreter (
                      :properties (
                                   :InterpreterPath ,(executable-find python-shell-interpreter)
                                   :DatabasePath ,ms-pyls-dir
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

  (lsp-define-stdio-client lsp-python "python"
                           (lsp-make-traverser #'(lambda (dir)
						                           (directory-files
						                            dir
						                            nil
						                            "setup.py")))
                           `("dotnet" ,(concat ms-pyls-dir "Microsoft.Python.LanguageServer.dll"))
                           :extra-init-params #'ms-pyls-extra-init-params)

  (add-hook 'python-mode-hook
            (lambda ()
              (lsp-python-enable))))

;; (def-package! lsp-mode
;;   :hook (lsp-after-open-hook . lsp-enable-imenu)
;;   :config
;;   (setq lsp-message-project-root-warning t))

;; (def-package! lsp-python
;;   :after (lsp-mode)
;;   :init (add-hook 'python-mode-hook #'lsp-python-enable))

(def-package! company-lsp
  :config
  (setq company-lsp-enable-recompletion t)
  (set-company-backend! 'lsp-mode 'company-lsp))

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (set-lookup-handlers! 'python-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 50
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-flycheck nil
        lsp-highlight-symbol-at-point nil
        lsp-ui-flycheck-enable nil))


;; (def-package! virtualenvwrapper
;;   :config
;;   (venv-initialize-interactive-shells)
;;   (venv-initialize-eshell))

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

(after! flycheck
  (setq flycheck-python-flake8-executable "flake8"
        flycheck-display-errors-delay 0.5))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match t))

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

     (:after lsp-ui
       :desc "Highlight symbol at point"  :n "h"  #'lsp-symbol-highlight
       :desc "Describe thing at point"    :n "/"  #'lsp-describe-thing-at-point
       ;; don't know why this stopped working
       (:map lsp-ui-mode-map
         :desc "Jump to definition"         :n "d"  #'lsp-ui-peek-find-definitions
         :desc "Jump to references"         :n "D"  #'lsp-ui-peek-find-references)))


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
