;; -*- lexical-binding: t -*-

(use-package whitespace
  :ensure t
  :hook (before-save . whitespace-cleanup)
  :config
  (setq whitespace-style '(face empty tabs lines-tail trailing)))

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (color-theme-sanityinc-tomorrow-night))
;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (projectile-mode))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(use-package idle-highlight-mode
  :ensure t
  :requires (clojure-mode)
  :hook clojure-mode)

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(use-package visual-regexp-steroids
  :ensure t
  :bind (("C-M-r" . vr/isearch-backward)
         ("C-M-s" . vr/isearch-forward)))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-S-c C-e" . mc/edit-ends-of-lines)
   ("C-S-c C-a" . mc/edit-beginnings-of-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-M-<return>" . mc/mark-more-like-this-extended)
   ("C-S-SPC" . set-rectangular-region-anchor)
   ("C-M-=" . mc/insert-numbers)
   ("C-*" . mc/mark-all-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package quickrun
  :ensure t
  :bind
  (("<f7>" . quickrun)
   ("<f8>" . quickrun-compile-only)))

(use-package move-text
  :ensure t
  :bind
  (("M-<up>" . move-text-up)
   ("M-<down>" . move-text-down)))

(use-package highlight-symbol
  :ensure t
  :bind (("<f3>" . highlight-symbol-at-point)
         ("M-<f3>" . highlight-symbol-remove-all))
  :config
  (setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1")))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1))

(use-package ag
  :ensure t
  :commands (ag)
  :config
  (setq ag-highlight-search t)
  (setq ag-group-matches nil))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package beacon
  ;; ref: https://github.com/Malabarba/beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package system-packages
  ;; ref: https://gitlab.com/jabranham/system-packages
  :ensure t)

(when (memq window-system '(mac ns x))
  ;; ref: https://github.com/purcell/exec-path-from-shell
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

(use-package hl-todo;; ref: https://github.com/tarsius/hl-todo
  :ensure t
  :demand t
  ;; :bind (("C-c t n" . hl-todo-next)
  ;;        ("C-c t p" . hl-todo-previous)
  ;;        ("C-c t o" . hl-todo-occur))
  :config (global-hl-todo-mode))

(use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

(use-package ruby-mode
  :ensure t
  :mode "Rakefile\\'")

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package graphviz-dot-mode
  ;; require company
  :ensure t
  :mode "\\.dot\\'")

(use-package web-mode
  :ensure t
  :after (add-node-modules-path)
  :mode
  (("\\.erb\\'" . web-mode)
   ("\\.js\\'" . web-mode)
   ("\\.jsx\\'" . web-mode)
   ("\\.json\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ;; TODO: Fix flycheck in order to use web-mode with .scss files
   ;; ("\\.scss\\'" . web-mode)
   ("\\.less\\'" . web-mode)
   ("\\.html\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.hbs\\'" . web-mode))
  :custom
  ;; Some from https://github.com/fxbois/web-mode/issues/872#issue-219357898
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-element-highlight t)

  ;; Indent inline JS/CSS within HTML
  ;; https://stackoverflow.com/a/36725155/3516664
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
  (web-mode-block-padding 2)
  (web-mode-comment-formats
   '(("java"       . "/*")
     ("javascript" . "//")
     ("php"        . "/*")
     ))
  :config
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-quotes" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (add-to-list 'web-mode-indentation-params '("case-extra-offset" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

  (define-key (current-local-map) (kbd "M-'") 'web-mode-comment-or-uncomment)

  (defun web-mode-tab-stop (width)
    "Set all web-mode tab stops to WIDTH in current buffer.

       Inspired by https://www.emacswiki.org/emacs/TabStopList
      and https://emacs.stackexchange.com/a/25046

     This updates `tab-stop-list', but not `tab-width'."
    (interactive "new web-mode tab stop: ")
    (setq web-mode-markup-indent-offset width)
    (setq web-mode-css-indent-offset width)

    (setq web-mode-code-indent-offset width)))

(use-package command-log-mode
    :ensure t)

(use-package yaml-mode
    :ensure t)

(use-package dockerfile-mode
    :ensure t)

(use-package docker-compose-mode
    :ensure t)

(when (display-graphic-p)
    (use-package all-the-icons :ensure t))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-height 19)
  (setq doom-modeline-bar-width 5)
  (setq doom-modeline-icon t)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-init))

(use-package doom-themes
  ;; ref: https://github.com/hlissner/emacs-doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t) ; default t, if nil, bold is universally disabled
  (setq doom-themes-enable-italic t) ; default t, if nil, italics is universally disabled
  (doom-themes-org-config)
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-visual-bell-config))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
