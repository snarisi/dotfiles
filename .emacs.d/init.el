;; Package setup

(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(defvar local-packages '(
			 projectile
			 auto-complete
			 epc
			 jedi
			 magit
			 git-gutter
			 helm
			 flycheck
			 virtualenvwrapper
			 fastnav
			 no-easy-keys
			 editorconfig
			 js2-mode
			 yasnippet
			 highlight-numbers
			 ))

(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p) (if (package-installed-p p nil) nil p)) packages)))

;; This delightful bit adapted from:
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/

(let ((need-to-install (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
	(package-install p)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#202020" "#DF5F5F" "#87AF5F" "#F0DFAF" "#8CD0D3" "#AF87AF" "#93E0E3" "##AFAFAF"])
 '(custom-safe-themes
   (quote
    ("0e1fc5a0476d05c1681e6f9ca84bddb94fa062fc3e4eb54f36c42708647ba3c6" "68a231d90b7001c4a481eabf6e3196cff1c4b43cc83200cdef9b3c71a3a03f83" "d3cf01a392661011c7dc3f4cf596c3796856177063b8e6c93a7483caea59f9fb" default)))
 '(fci-rule-color "#383838")
 '(git-gutter:update-interval 2)
 '(nrepl-message-colors
   (quote
    ("#DF5F5F" "#DFAF87" "#F0DFAF" "#87AF5F" "#BFEBBF" "#93E0E3" "#94BFF3" "#AF87AF")))
 '(package-selected-packages
   (quote
    (yasnippet highlight-numbers fastnav smart-tabs-mode virtualenvwrapper helm-projectile helm projectile magit jedi git-gutter-fringe+ git-gutter diff-hl)))
 '(pdf-view-midnight-colors (quote ("##AFAFAF" . "#383838")))
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#DF5F5F")
     (60 . "#DFAF87")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#87AF5F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#AF87AF"))))
 '(vc-annotate-very-old-color "#AF87AF"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'general-config)
(require 'jedi-starter)
(require 'navigation)
(require 'underscore)

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
