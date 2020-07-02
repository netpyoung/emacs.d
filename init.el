;;; package --- "init.el"
;;; Commentary:
;;;   -  https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
;;;   - https://www.emacswiki.org/emacs/YesOrNoP
;;; Code:


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; package
;;    - [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html][Package]]
;;    - https://melpa.org/


(require 'package)
(setq package-archives
      '(
        ("gnu" . "https://elpa.gnu.org/packages/")
        ;; officiral - melpa
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ;; mirror - melpa
        ;; ("melpa" . "http://www.mirrorservice.org/sites/melpa.org/packages/")
        ;; ("melpa-stable" . "http://www.mirrorservice.org/sites/stable.melpa.org/packages/")
        ;; org-mode
        ("org" . "http://orgmode.org/elpa/")
        ))
(setq package-user-dir "~/.emacs.d/__package")
(setq package-check-signature nil)
(package-initialize)

;; use-package
;;    - https://github.com/jwiegley/use-package
;;    - use ~:pin melpa-stable~ for stable.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package init-loader :ensure t
  :init
  (setq init-loader-directory "~/.emacs.d/conf.d")
  ;; (setq init-loader-byte-compile t)
  (init-loader-load))
