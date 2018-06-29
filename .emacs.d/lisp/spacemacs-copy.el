;; for smooter UI at startup
;; (hidden-mode-line-mode)

;; removes the GUI elements
;; (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
;;   (tool-bar-mode -1))
;; (unless (spacemacs/window-system-is-mac)
;;   (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
;;     (menu-bar-mode -1)))
;; (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
;;   (scroll-bar-mode -1))
;; ;; tooltips in echo-aera
;; (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
;;   (tooltip-mode -1)))

;; ido vertical mode?

(defvar custom-default-font '("Source Code Pro"
			      :size 13
			      :weight normal
			      :width normal
			      :powerline-scale 1.1))
(defun set-default-font (plists)
  "Set the font given the passed PLISTS.

PLISTS has either the form (\"fontname\" :prop1 val1 :prop2 val2 ...)
or is a list of such. The first font that can be found will be used.

The return value is nil if no font was found, truthy otherwise."
  (unless (listp (car plists))
    (setq plists (list plists)))
  (catch 'break
    (dolist (plist plists)
      (when (find-font (font-spec :name (car plist)))
        (let* ((font (car plist))
               (props (cdr plist))
               (scale (plist-get props :powerline-scale))
               (font-props (spacemacs/mplist-remove
                            (spacemacs/mplist-remove props :powerline-scale)
                            :powerline-offset))
               (fontspec (apply 'font-spec :name font font-props)))
          (spacemacs-buffer/message "Setting font \"%s\"..." font)
          (set-frame-font fontspec nil t)
          (push `(font . ,(frame-parameter nil 'font)) default-frame-alist)
          (setq-default powerline-scale scale)
          (setq-default powerline-height (spacemacs/compute-powerline-height))
          ;; fallback font for unicode characters used in spacemacs
          (pcase system-type
            (`gnu/linux
             (setq fallback-font-name "NanumGothic")
             (setq fallback-font-name2 "NanumGothic"))
            (`darwin
             (setq fallback-font-name "Arial Unicode MS")
             (setq fallback-font-name2 "Arial Unicode MS"))
            (`windows-nt
             (setq fallback-font-name "MS Gothic")
             (setq fallback-font-name2 "Lucida Sans Unicode"))
            (`cygwin
             (setq fallback-font-name "MS Gothic")
             (setq fallback-font-name2 "Lucida Sans Unicode"))
            (other
             (setq fallback-font-name nil)
             (setq fallback-font-name2 nil)))
          (when (and fallback-font-name fallback-font-name2)
            ;; remove any size or height properties in order to be able to
            ;; scale the fallback fonts with the default one (for zoom-in/out
            ;; for instance)
            (let* ((fallback-props (spacemacs/mplist-remove
                                    (spacemacs/mplist-remove font-props :size)
                                    :height))
                   (fallback-spec (apply 'font-spec
                                         :name fallback-font-name
                                         fallback-props))
                   (fallback-spec2 (apply 'font-spec
                                          :name fallback-font-name2
                                          fallback-props)))
              ;; window numbers
              (set-fontset-font "fontset-default"
                                '(#x2776 . #x2793) fallback-spec nil 'prepend)
              ;; mode-line circled letters
              (set-fontset-font "fontset-default"
                                '(#x24b6 . #x24fe) fallback-spec nil 'prepend)
              ;; mode-line additional characters
              (set-fontset-font "fontset-default"
                                '(#x2295 . #x22a1) fallback-spec nil 'prepend)
              ;; new version lighter
              (set-fontset-font "fontset-default"
                                '(#x2190 . #x2200) fallback-spec2 nil 'prepend))))
        (throw 'break t)))
    nil))

(defvar custom-default-font '("Source Code Pro"
                                    :size 13
                                    :weight normal
                                    :width normal
                                    :powerline-scale 1.1))

 (set-default-font custom-default-font)

 (provide 'spacemacs-copy)