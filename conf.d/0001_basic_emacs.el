;; -*- lexical-binding: t -*-

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setenv "LANG" "en_US.UTF-8")

(require 'server)
;; (setq server-name (format-time-string "%Y-%m-%dT%H:%M:%S"))
;; (setq server-socket-dir "~/.emacs.d/__server")
(unless (server-running-p)
  (server-start))

(setq load-prefer-newer t)
(customize-set-variable 'tramp-default-method "ssh")


(setq system-time-locale "C")

;; [== MacOSX ==]
(setq mac-command-modifier 'meta)

;; [== Keys ==]
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-x C-.") 'start-kbd-macro)
(global-set-key (kbd "C-x C-,") 'end-kbd-macro)
(global-set-key (kbd "C-x C-/") 'call-last-kbd-macro)
(global-set-key (kbd "<C-tab>") 'bury-buffer)


;; [== language ==]
(setq default-input-method "korean-hangul3f")

;; [== windows ==]
(setq frame-title-format '("[%*] %f"))
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)


;; [== on/off modes ==]
(display-time-mode 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)


;; [== gc ==]
(setq gc-cons-threshold (* 1024 1024 100))

;; Faster rendering by not corresponding to right-to-left language
(setq-default bidi-display-reordering nil)

;; ref: http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-thre;; shold/
;; (defun my-minibuffer-setup-hook ()
;;   (setq gc-cons-threshold most-positive-fixnum))

;; (defun my-minibuffer-exit-hook ()
;;   (setq gc-cons-threshold 800000))

;; (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)


;; [== aliasing ==]
(defalias 'list-buffers 'ibuffer)


;; [== indent ==]
(setq-default indent-tabs-mode nil)
(setq tab-width 4)


;; [== highlighting ==]
(defvar show-paren-style 'expression)
(global-hl-line-mode t)

;; [== etc ==]
(add-to-list 'safe-local-variable-values
             '(lexical-binding . t))

(defvar grep-find-use-grep-r nil)
(defvar grep-highlight-matches 'auto-detect)

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;; Scroll
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))


;; transparent
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(80 . 50) '(100 . 100)))))

(global-set-key (kbd "C-c t") 'toggle-transparency)

;; warning
(setq visible-bell nil)
(add-to-list 'exec-path "/usr/local/bin")

(require 'paren)
(setq show-paren-style 'expression)
(show-paren-mode t)

;; projectile bug...
(require 'subr-x)

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/__auto-save-list" t)))

(setq backup-directory-alist '(("." . "~/.emacs.d/__backups")))

(setq eshell-directory-name "~/.emacs.d/__eshell")

(setq savehist-file "~/.emacs.d/__savehist")
(savehist-mode 1)

(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
