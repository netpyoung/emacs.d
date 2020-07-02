(use-package go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook 'company-mode))


(use-package company-go
  :ensure t
  :init
  (add-to-list 'company-backends 'company-go))


(use-package go-stacktracer
  :ensure t)

(use-package go-add-tags
  :ensure t)

(use-package go-eldoc
  :ensure t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-gopath
  :ensure t)

(use-package go-direx
  :ensure t)

(use-package gotest
  :ensure t)

(use-package go-playground
  :ensure t)
