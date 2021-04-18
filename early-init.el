
(let ((default-directory (file-name-directory load-file-name)))

  ;; Over-ride to use ./profiles.el as the one and only profiles file
  (setq chemacs-profiles-paths (list (expand-file-name "profiles.el")))

  (load (expand-file-name "chemacs2/early-init.el")))
