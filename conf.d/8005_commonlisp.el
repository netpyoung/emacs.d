;; [scoop](https://scoop.sh/)
;; [sbcl](http://www.sbcl.org/)
;; [quicklisp](https://www.quicklisp.org/)

;; windows - powershell
;; set-executionpolicy remotesigned -s currentuser
;; iex (new-object net.webclient).downloadstring('https://get.scoop.sh')
;; scoop install aria2
;; scoop install gow
;; scoop install sbcl

;; mkdir sbcl && cd sbcl
;; sbcl> curl -O https://beta.quicklisp.org/quicklisp.lisp
;; sbcl> sbcl --load quicklisp.lisp
;; * (quicklisp-quickstart:install)
;; * (ql:add-to-init-file)
;; * (quit)
;; sbcl> sbcl
;; * (print "Helloworld")
;; "Helloworld"
;; "Helloworld"
;; * (quit)

;; (ql:quickload "slynk")
;; (slynk:create-server :port 4008)

(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program (string-trim (shell-command-to-string "which sbcl"))))
