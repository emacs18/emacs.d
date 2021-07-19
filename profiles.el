;; This specifies emacs profiles select-able on emacs startup
;; with --with-profile option.
;;
;; - 'car' of each sub-list is the profile name.
;; - 'cdr' of each sub-list specifies elisp variable name/value pairs.
;;
;; In most cases only user-emacs-directory needs to be set.

(
 ("default"      . ((user-emacs-directory . "~/.emacs.d/sm-m/")
                    (env . (("SPACEMACSDIR" . "~/.emacs.d/sm-m/.spacemacs")))))

 ;; - sm-ms: use straight.el and my setup; this is default
 ;; - sm-d : vanilla develop branch; uses package.el
 ;; - sm-m : my setup, use pacakge.el
 ;; - sm-s:  vanilla develop branch except to use straight.el
 ("sm-ms" . ((user-emacs-directory . "~/.emacs.d/sm-ms/")
             (env . (("SPACEMACSDIR" . "~/.emacs.d/sm-ms/.spacemacs")))))
 ("sm-d" . ((user-emacs-directory . "~/.emacs.d/sm-d/")
            (env . (("SPACEMACSDIR" . "~/.emacs.d/sm-d/.spacemacs")))))
 ("sm-m" . ((user-emacs-directory . "~/.emacs.d/sm-m/")
            (env . (("SPACEMACSDIR" . "~/.emacs.d/sm-m/.spacemacs")))))
 ("sm-s" . ((user-emacs-directory . "~/.emacs.d/sm-s/")
            (env . (("SPACEMACSDIR" . "~/.emacs.d/sm-s/.spacemacs")))))

 ;; "-s" suffix denotes versions that use straight.el.
 ;; doom already uses straight.el
 ("doom"         . ((user-emacs-directory . "~/.emacs.d/doom/")
                    (env . (("DOOMDIR" . "~/.emacs.d/doom/.doom.d")))))

 ("purcell"      . ((user-emacs-directory . "~/.emacs.d/purcell/")))
 ("purcell-s"    . ((user-emacs-directory . "~/.emacs.d/purcell-s/")))

 ("scimax"       . ((user-emacs-directory . "~/.emacs.d/scimax/")))
 ("scimax-s"     . ((user-emacs-directory . "~/.emacs.d/scimax-s/")))

 ("ve"           . ((user-emacs-directory . "~/.emacs.d/vanilla-emacs/")))
 )
