;; -*- no-byte-compile: t; -*-
;;; config/snarisi/packages.el

(package! evil-easymotion)
(package! lsp-ui)
(package! company-lsp)
(package! company-box)
(package! lsp-python)
(package! eglot)
(package! virtualenvwrapper)
(package! pyvenv)
(package! bash-completion)
(package! quasi-monochrome-theme)
(package! multiple-cursors)
(package! dockerfile-mode)
(package! docker)
(package! anaconda-mode :ignore t)
(disable-packages! anaconda-mode)
(package! evil-matchit :recipe (:fetcher github :repo "redguardtoo/evil-matchit" :commit "7d65b4167b1f0086c2b42b3aec805e47a0d355c4"))
