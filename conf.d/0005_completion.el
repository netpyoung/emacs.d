;; [https://company-mode.github.io/
;; http://ohyecloudy.com/emacsian/2017/08/05/package-helm-ag-ripgrep-windows/

(use-package company
  :ensure t
  :config
  ;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay nil)
  (setq company-show-numbers true)
  (global-company-mode))



;; https://github.com/emacs-helm/helm
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(use-package ido
  :ensure t)

(use-package flx-ido
  :ensure t
  :config
  (ido-mode -1)
  ;; (ido-everywhere 1)
  (flx-ido-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; [== helm ==]
(use-package helm
  :ensure t
  :requires (ido)
  :bind
  (("C-x C-x" . execute-extended-command)
   ("M-x" . helm-M-x))
  :config
  (require 'helm-config)
  (setq helm-candidate-number-limit 100)
  ;; From https://gist.github.com/antifuchs/9238468
  (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
        helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
        helm-yas-display-key-on-candidate t
        helm-quick-update t
        helm-M-x-requires-pattern nil
        helm-ff-skip-boring-files t)
  (helm-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . ido)))

(use-package projectile
  :bind ("C-x p" . projectile-find-file)
  :ensure t)

(use-package helm-projectile
  :ensure t
  :bind ("C-x p" . helm-projectile))

(use-package helm-swoop
  :ensure t
  :bind
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   :map
   helm-swoop-map
   ("M-i" . helm-multi-swoop-all-from-helm-swoop)
   ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop)))

(use-package helm-ag
  ;; ref: https://github.com/syohex/emacs-helm-ag
  ;; ref: http://ohyecloudy.com/emacsian/2017/08/02/package-helm-ag-basic/
  :ensure t)

(use-package helm-rg
  ;; ref: https://github.com/microamp/helm-rg
  :ensure t)

;; deprecated - ivy
;; ;; -*- lexical-binding: t -*-
;; (use-package ivy :ensure t
;;   :diminish (ivy-mode . "")
;;   :requires (projectile)
;;   :bind ("C-x p" . projectile-find-file)
;;   :config
;;   (setq projectile-completion-system 'ivy)
;;   (ivy-mode 1)
;;   ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
;;   (setq ivy-use-virtual-buffers t)
;;   (setq projectile-completion-system 'ivy)
;;   ;; number of result lines to display
;;   (setq ivy-height 40)
;;   (setq ivy-count-format "(%d/%d) ")
;;   ;; no regexp by default
;;   (setq ivy-initial-inputs-alist nil)
;;   ;; configure regexp engine.
;;   (setq ivy-re-builders-alist
;;         ;; allow input not in order
;;         '((t   . ivy--regex-ignore-order))))
;; (use-package counsel
;;   :ensure t
;;   :bind*
;;   (("C-x C-x" . execute-extended-command)
;;    ("M-x" . counsel-M-x)
;;    ("C-x C-f" . counsel-find-file))
;;   :custom
;;   (counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))
;; (use-package counsel-projectile
;;   :ensure t
;;   :config
;;   (counsel-projectile-mode))
;; (use-package amx
;;   :ensure t
;;   :config (amx-mode t))
