(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program (string-trim (shell-command-to-string "which sbcl"))))
