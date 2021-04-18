;; This specifies emacs profiles select-able on emacs startup
;; with --with-profile option.
;;
;; - 'car' of each sub-list is the profile name.
;; - 'cdr' of each sub-list specifies elisp variable name/value pairs.
;;
;; In most cases only user-emacs-directory needs to be set.

(
 ("default"      . ((user-emacs-directory . "~/.emacs.d/my-spacemacs/")
                    (env . (("SPACEMACSDIR" . "~/.emacs.d/my-spacemacs/.spacemacs")))))

 ;; - my-spacemacs: for daily use; uses straight.el; this is default
 ;; - spacemacs:    Vanilla develop branch; uses package.el
 ;; - spacemacs-s:  Vanilla develop branch except to use straight.el
 ("my-spacemacs" . ((user-emacs-directory . "~/.emacs.d/my-spacemacs/")
                    (env . (("SPACEMACSDIR" . "~/.emacs.d/my-spacemacs/.spacemacs")))))
 ("spacemacs"    . ((user-emacs-directory . "~/.emacs.d/spacemacs/")
                    (env . (("SPACEMACSDIR" . "~/.emacs.d/spacemacs/.spacemacs")))))
 ("spacemacs-s"  . ((user-emacs-directory . "~/.emacs.d/spacemacs-s/")
                    (env . (("SPACEMACSDIR" . "~/.emacs.d/spacemacs-s/.spacemacs")))))

 ;; "-s" suffix denotes versions that use straight.el.
 ;; doom already uses straight.el
 ("doom"         . ((user-emacs-directory . "~/.emacs.d/doom/")
                    (env . (("DOOMDIR" . "~/.emacs.d/doom/.doom.d")))))

 ("purcell"      . ((user-emacs-directory . "~/.emacs.d/purcell/")))
 ("purcell-s"    . ((user-emacs-directory . "~/.emacs.d/purcell-s/")))

 ("scimax"       . ((user-emacs-directory . "~/.emacs.d/scimax/")))
 ("scimax-s"     . ((user-emacs-directory . "~/.emacs.d/scimax-s/")))

 ("vanilla"      . ((user-emacs-directory . "~/.emacs.d/vanilla/")))
 )
