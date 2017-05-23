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
			 exec-path-from-shell
			 powerline
			 org
			 desktop+
			 multiple-cursors
			 smart-mode-line
			 smart-mode-line-powerline-theme
			 readline-complete
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
    ("48be3ae76022c204ce989159161ff247e0d4c600b6ec51290b2bd84a9cf6b8b9" "4e6b56ddacf26311f86f9d7b9a033d6543a11dec4847cb01576ce57deda63de1" "21631b14c53f3c37bf6d0dc11bc7167c8546337c74bebb88cc3531b513cf45b6" "82efad23e89715fa12ea9c537eac474d981e866765965fe304b56709edd41958" "5875a9e750c26133f43dd6bcf83b6ba4a5bbbf16e8410ff024141b8bd0a4e8fa" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "db72ce2042e725e4d6d1751e75c06f145418bd541bea214f77a178a0220e6a15" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "0e1fc5a0476d05c1681e6f9ca84bddb94fa062fc3e4eb54f36c42708647ba3c6" "68a231d90b7001c4a481eabf6e3196cff1c4b43cc83200cdef9b3c71a3a03f83" "d3cf01a392661011c7dc3f4cf596c3796856177063b8e6c93a7483caea59f9fb" default)))
 '(fci-rule-color "#383838")
 '(git-gutter:update-interval 2)
 '(nrepl-message-colors
   (quote
    ("#DF5F5F" "#DFAF87" "#F0DFAF" "#87AF5F" "#BFEBBF" "#93E0E3" "#94BFF3" "#AF87AF")))
 '(package-selected-packages
   (quote
    (readline-complete multiple-cursors desktop+ helm-ag web-mode helm-spotify smart-mode-line smart-mode-line-powerline-theme yasnippet highlight-numbers fastnav smart-tabs-mode virtualenvwrapper helm-projectile helm projectile magit jedi git-gutter-fringe+ git-gutter diff-hl)))
 '(pdf-view-midnight-colors (quote ("##AFAFAF" . "#383838")))
 '(sml/mode-width
   (if
       (eq
	(powerline-current-separator)
	(quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (car powerline-default-separator-dir)))
		   (quote powerline-active1)
		   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (cdr powerline-default-separator-dir)))
		   (quote powerline-active1)
		   (quote sml/global))))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (car powerline-default-separator-dir)))
		   (quote sml/global)
		   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
		  (quote display)
		  (funcall
		   (intern
		    (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (cdr powerline-default-separator-dir)))
		   (quote powerline-active2)
		   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
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
 '(default ((t (:height 120 :family "Fira Mono for Powerline")))))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'general-config)
(require 'navigation)
(require 'underscore)

;; spacemacs
;; (setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
;; (load-file (concat spacemacs-start-directory "init.el"))

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
