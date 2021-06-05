;;; init-straight.el --- setup to use straight.el rather than package.el
;;
;; Copyright (c) 2021 Richard Kim
;;
;; Author: Richard Kim <emacs18@gmail.com>
;; URL: https://github.com/emacs18
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; This should be loaded very early on before any packages are installed to
;;
;; - initialize straight.el to install packages under ~/.emacs.d/straight/
;; - install use-package and over-ride it so that it will use straight.el
;;   rather than package.el
;; - install use-package, diminish, and org-plus-contrib packages
;; - advise package-install and related functions so that these too will use
;;   straight.el
;;
;; This file is loaded by early-init.el of s-spacemacs, s-scimax, and s-purcell
;; packages. See Makefile for more info.

;; straight-base-dir should be set prior to loading this file. Typically it is
;; set to ".local" sub-directory of each configuration, e.g., following is found
;; in ./s-scimax/early-init.el, ./s-purcell/early-init.el, etc.:
;;
;; (setq straight-base-dir
;;       (expand-file-name ".local" (file-name-directory load-file-name)))
;;
;; I chose to use .local directory, because that is used by doom. Using this
;; convention seemed reasonable.

;; Install packages under build-<EMACS_VERSION> sub-directory of straight-base-dir.
(setq straight-build-dir (format "build-%s" emacs-version))

;; Use "develop" rather than default "master" to use bleeding edge version of
;; straight.el.
(setq straight-repository-branch "develop")

;; Setup so that (use-package ...) will use straight.el rather than package.el
;; by default. If this is not desired, then one can go back to using package.el
;; for specific instances by adding ":straight nil", e.g., (use-package
;; my-custom-package :straight nil ...).
(setq straight-use-package-by-default t)

;; Default value is '(find-at-startup find-when-checking). I removed
;; find-at-startup to prevent automatic updating of packages on emacs startup. I
;; would rather update all packages manually at the time of my choosing.
(setq straight-check-for-modifications '(find-when-checking))

;; Default is 'full which means to clone complete history. This could be set to
;; 1 to minimize disks usage. However I set this to 'full for the benefit of
;; profiles such as doom which may request a specific version which may be quite
;; old.
(setq straight-vc-git-default-clone-depth 1)

;; 'https is the default, but can be set to 'ssh
(setq straight-vc-git-default-protocol 'https)

;; Normally straight.el does not need to know your user names on github, gitlab,
;; etc. However you may need to specify it if you forked a package, and you fork
;; is at github or other sites.
(setq straight-host-usernames
      '((github . "githubuser")
        (gitlab . "gitlabUser")
        (bitbucket . "bitbucketUser")))

;; A profile is collection of package names and git version numbers. Thus a
;; profile identifies a very accurate state of installed packages. You can
;; create profiles by "freezing" current versions by calling
;; `straight-freeze-versions`.
(setq straight-profiles
      '((nil . "default.el")
        ;; You can any any number of additional profiles, e.g.,
        ;; (profile1 . "profile1.el")
        ))

;; You set set which profile to use.
;; (setq straight-current-profile 'profile1)

;; Following few lines are standard bootstrap setup lines from
;; https://github.com/raxod502/straight.el.git
;; except use of straight-base-dir rather than user-home-directory.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install few widely used packages.
(straight-use-package 'use-package) ; this is needed pretty much by all configuations
(straight-use-package 'diminish)    ; this is needed by many setups, e.g., scimax

;; If recipes are to be over-ridden, then it should be done very early on before
;; the default recipe is used, e.g., right about here.  Following is an example
;; to use my own forked hook-helpers with a bug fix.
;;
;; (straight-override-recipe
;;  '(hook-helpers :type git :host github :repo "emacs-straight/hook-helpers"
;;                 :fork (:host github :repo "emacs18/hook-helpers")
;;                 :branch "site" :files ("*" (:exclude ".git"))))

;; Another reason to use custom recipe is to work around internet access
;; problems. For example if your company does not allow access to ".cc" domain,
;; but allows access to github, then you might want to uncomment this, because
;; url for the package is https://depp.brause.cc/eyebrowse.git.
;;
;; (straight-override-recipe '(eyebrowse :host github :repo "emacsmirror/eyebrowse"))

(straight-override-recipe '(devdocs :host github :repo "astoff/devdocs.el"))

'(straight-override-recipe
 `(org :type git
       :repo "https://code.orgmode.org/bzg/org-mode.git"
       :local-repo "org"
       :depth full
       :pre-build ,(list
                    (concat
                     (when
                         (eq
                          system-type
                          'berkeley-unix) "g")
                     "make")
                    "autoloads"
                    "./doc/org.texi"
                    "./doc/orgguide.texi"
                    (concat "EMACS="
                            invocation-directory invocation-name))
       :build (:not autoloads)
       :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*") "doc/*.texi")))

;; How do you prevent installing 'org' package in addition to
;; 'org-plus-contrib'? Following seems to be the answer. This may need to be
;; tweaked when org 9.5 version comes out which splits up org and contrib parts
;; into two. This issue is discussed here:
;; https://github.com/raxod502/straight.el/issues/352
;; https://github.com/raxod502/straight.el/issues/624
;;
(straight-use-package 'org)

;; Few package.el functions are advised below to allow spacemacs, scimax, and
;; other configurations to use straight.el rather than package.el.

(defadvice package-install (around use-straight activate)
  "Use straight.el to install packages rather than package.el."
  (let ((pkg (ad-get-arg 0)))
    (straight-use-package (if (symbolp pkg) pkg (package-desc-name pkg)))))

(defun my-installed-by-straight-p (pkg)
  "Return `non-nil' if PKG was installed by straight.el package manager."
  (let ((pkg-name (if (symbolp pkg) (symbol-name pkg) pkg)))
    (and (fboundp 'straight--build-file)
         (file-directory-p (straight--build-file pkg-name)))))

(defadvice package-installed-p (around use-straight activate)
  "Check first whether straight.el has installed the package."
  (if (my-installed-by-straight-p (ad-get-arg 0))
      (setq ad-return-value t)
    ad-do-it))

(defun my-package-activate-via-straight (origfunc &rest args)
  "If package to be activated was installed by straight.el
package manager, then simply call `straight-use-package' to make
sure the package is installed and activated."
  (let* ((arg0 (car args))
         (pkg-sym (if (symbolp arg0)
                      arg0
                    (if (stringp arg0)
                        (intern arg0)
                      (package-desc-name arg0)))))
    (if (my-installed-by-straight-p pkg-sym)
        ;; FIXME: How should packages be activated?
        (straight-use-package pkg-sym)
      (apply origfunc args))))

(advice-add 'package-activate :around #'my-package-activate-via-straight)

(defvar my-straight-in-use t
  "This is non-nil if emacs was configured to use straight.el package manager.")
