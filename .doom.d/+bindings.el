;;; config/snarisi/+bindings.el -*- lexical-binding: t; -*-

(map!
 (:leader
   :desc "Easymotion next"  :n "j"  #'evilem-motion-next-line
   :desc "Easymotion prev"  :n "k"  #'evilem-motion-previous-line

   (:desc "search" :prefix "/"
     :desc "Counsel-ag"            :nv "g" #'counsel-ag)

   (:desc "code" :prefix "c"
     :desc "Pop marker stack"      :n ","  #'xref-pop-marker-stack

     ;; this should be :after venv-mode or whatever
     :desc "Change virtualenv"     :n "v"  #'venv-workon))

 ;; re-add default emacs bindings as I find things that are annoying
 (:map evil-insert-state-map
   "C-n"   #'next-line
   "C-p"   #'previous-line
   "C-b"   #'backward-char
   "C-f"   #'forward-char
   "M-b"   #'backward-word
   "M-f"   #'forward-word
   "M-d"   #'kill-word
   "C-w"   #'kill-region
   "M-w"   #'ns-copy-including-secondary
   "C-SPC" #'set-mark-command
   "C-k"   #'kill-line
   "C-d"   #'delete-char))
