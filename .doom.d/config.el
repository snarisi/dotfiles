;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(load! "+bindings")

;; (require 'quasi-monochrome-theme)
;; (setq doom-theme 'quasi-monochrome)
;; (setq doom-theme 'doom-one)
(solaire-mode-swap-bg)
(setq doom-font (font-spec :family "Ubuntu Mono" :size 17))
(setq doom-big-font (font-spec :family "Ubuntu Mono" :size 19))

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

(cond (IS-MAC
       (setq mac-option-modifier 'meta
             mac-command-modifier 'super)))

(def-package! lsp-mode
  :hook (lsp-after-open-hook . lsp-enable-imenu)
  :config
  (lsp-define-stdio-client lsp-python "python"
			   (lsp-make-traverser #'(lambda (dir)
						   (directory-files
						    dir
						    nil
						    "setup.py")))
			   '("pyls"))
  (add-hook 'python-mode-hook #'lsp-python-enable))

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

;; (def-package! company-box
;;   :hook (company-mode . company-box-mode))

(after! flycheck
  (setq flycheck-python-flake8-executable "flake8"
        flycheck-display-errors-delay 0.2))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2))
