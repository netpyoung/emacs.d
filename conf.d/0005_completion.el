(use-package company
  :ensure t
  :config
  ;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay nil)
  (setq company-show-numbers true)
  (global-company-mode))

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
