;;; fastnav-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "fastnav" "fastnav.el" (22738 39651 0 0))
;;; Generated autoloads from fastnav.el

(autoload 'fastnav-search-char-forward "fastnav" "\
Moves to the arg-th occurrence of char forward (backward if N
is negative).  If there isn't room, go as far as possible (no
error).

\(fn ARG CHAR)" nil nil)

(autoload 'fastnav-search-char-backward "fastnav" "\
Moves to the arg-th occurrence of char backward (forward if N
is negative).  If there isn't room, go as far as possible (no
error).

\(fn ARG CHAR)" nil nil)

(autoload 'fastnav-get-nth-chars "fastnav" "\
Computes and returns the positions of the ARG'th occurrence of
characters of the range 1 .. 255.

\(fn ARG)" nil nil)

(autoload 'fastnav-highlight-read-char "fastnav" "\
Highlights the ARG'th occurences of each character while
querying one using message TEXT. Negative ARG means backward
search of occurences.

\(fn TEXT ARG FORWARDER BACKWARDER)" nil nil)

(autoload 'fastnav-highlight-read-char-backward "fastnav" "\
Highlights the backward ARG'th occurences of each character
while querying one using message TEXT.

\(fn TEXT ARG FORWARDER BACKWARDER)" nil nil)

(autoload 'fastnav-jump-to-char-forward "fastnav" "\
Jump to the ARG'th occurence of a character that is queried
interactively while highlighting the possible positions.

\(fn ARG)" t nil)

(autoload 'fastnav-jump-to-char-backward "fastnav" "\
Jump backward to the ARG'th occurence of a character that is
queried interactively while highlighting the possible positions.

\(fn ARG)" t nil)

(autoload 'fastnav-mark-up-to-char-forward "fastnav" "\
Set mark before the ARG'th occurence of a character queried
interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-mark-up-to-char-xbackward "fastnav" "\
Set mark backward after the ARG'th occurence of a character
queried interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-mark-to-char-forward "fastnav" "\
Set mark before the ARG'th occurence of a character queried
interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-mark-to-char-backward "fastnav" "\
Set mark backward after the ARG'th occurence of a character
queried interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-zap-up-to-char-forward "fastnav" "\
Kill text up to the ARG'th occurence of a character queried
interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-zap-up-to-char-backward "fastnav" "\
Kill text backward to the ARG'th occurence of a character
queried interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-zap-to-char-forward "fastnav" "\
Kill text up to and including the ARG'th occurence of a character queried
interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-zap-to-char-backward "fastnav" "\
Kill text backward to the ARG'th occurence of a character
queried interactively.

\(fn ARG)" t nil)

(autoload 'fastnav-replace-char-forward "fastnav" "\
Interactively replaces the ARG'th occurence of a character.

\(fn ARG)" t nil)

(autoload 'fastnav-replace-char-backward "fastnav" "\
Interactively replaces the ARG'th backward occurence of a
character.

\(fn ARG)" t nil)

(autoload 'fastnav-insert-at-char-forward "fastnav" "\
Queries for a character and a string that is insterted at
the ARG'th occurence of the character.

\(fn ARG)" t nil)

(autoload 'fastnav-insert-at-char-backward "fastnav" "\
Queries for a character and a string that is insterted at
the backward ARG'th occurence of the character.

\(fn ARG)" t nil)

(autoload 'fastnav-execute-at-char-forward "fastnav" "\
Queries for a character and a key sequence that is executed at
the ARG'th occurence of the character.

\(fn ARG)" t nil)

(autoload 'fastnav-execute-at-char-backward "fastnav" "\
Queries for a character and a key sequence that is executed at
the backward ARG'th occurence of the character.

\(fn ARG)" t nil)

(autoload 'fastnav-delete-char-forward "fastnav" "\
Deletes the ARG'th occurence of a character, which is queried
interactively while highlighting the possible positions.

\(fn ARG)" t nil)

(autoload 'fastnav-delete-char-backward "fastnav" "\
Deletes the backward ARG'th occurence of a character, which is
queried interactively while highlighting the possible positions.

\(fn ARG)" t nil)

(autoload 'fastnav-sprint-forward "fastnav" "\
Performs a sequence of jumping forward to the next character
matching the keyboard event.

\(fn ARG)" t nil)

(autoload 'fastnav-sprint-backward "fastnav" "\
Performs a sequence of jumping backward to the next character
matching the keyboard event.

\(fn ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; fastnav-autoloads.el ends here
