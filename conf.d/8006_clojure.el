(use-package cider
  ;; ref: https://github.com/clojure-emacs/cider
  ;; ref: https://cider.readthedocs.io/en/latest/interactive_programming/
  :ensure t
  :pin melpa-stable
  :requires company
  :bind
  ;; https://github.com/clojure-emacs/cider/blob/master/doc/interactive_programming.md
  ;; https://github.com/clojure-emacs/cider/blob/master/cider.el
  ;; debugging
  ;; TODO(pyoung): need to study.
  ;; ref: https://cider.readthedocs.io/en/stable/debugging/
  (("C-M-i" . company-complete)
   ("C-M-t" . projectile-toggle-between-implementation-and-test))

  :config
  ;; https://github.com/clojure-emacs/cider/blob/master/doc/configuration.md

  ;; overriding default cider-find-and-clear-repl-output to forcing clear!
  (define-key cider-mode-map (kbd "C-c C-o")
    '(lambda () (interactive) (cider-find-and-clear-repl-output t)))
  (define-key cider-repl-mode-map (kbd "C-c C-o")
    'cider-repl-clear-buffer)

  ;; I don't like paredit.
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'turn-on-eldoc-mode)

  (setq cider-popup-stacktraces nil)
  (setq cider-repl-popup-stacktraces t)
  (setq cider-auto-jump-to-error nil)
  (setq nrepl-buffer-name-show-port t)

  (setq cider-overlays-use-font-lock t)
  (setq clojure-indent-style 'align-arguments)

  ;; for cider-eval-defun-to-comment
  (setq cider-eval-result-prefix ";; => ")

  ;; for cider-doc
  (setq cider-prompt-for-symbol nil)

  (setq cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t
        cider-repl-history-size 10000
        cider-prefer-local-resources t)

  (defun cider-reset ()
    (interactive)
    (cider-interactive-eval "(user/reset)"))

  (defun cider-refresh ()
    (interactive)
    (cider-interactive-eval "(require '[clojure.tools.namespace.repl]) (clojure.tools.namespace.repl/refresh)"))

  (define-key cider-repl-mode-map (kbd "C-c C-x C-c C-r")
    'cider-refresh))


(use-package clojure-mode
  :ensure t
  :requires cider
  :bind ("C-M-i" . company-complete)
  :config
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

  ;; (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (define-clojure-indent
    (implement '(1 (1)))
    (letfn     '(1 ((:defn)) nil))
    (proxy     '(2 nil nil (1)))
    (reify     '(:defn (1)))
    (deftype   '(2 nil nil (1)))
    (defrecord '(2 nil nil (1)))
    (specify   '(1 (1)))
    (specify   '(1 (1)))
    ;;
    (fn-traced '(1 (1))))

  (define-key clojure-mode-map (kbd "C-;") 'mark-sexp)
  (customize-set-variable 'clojure-align-forms-automatically t)

  ;; eye-candy
  (add-to-list 'auto-coding-alist '("\\.clj\\'" . utf-8))
  (add-to-list 'auto-coding-alist '("\\.cljs\\'" . utf-8))
  (add-to-list 'auto-coding-alist '("\\.cljx\\'" . utf-8))

  (font-lock-add-keywords
   'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "λ")
                              nil)))))

  (font-lock-add-keywords
   'clojure-mode `(("\\(#\\)("
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "ƒ")
                              nil)))))

  (font-lock-add-keywords
   'clojure-mode `(("\\(#\\){"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "∈")
                              nil)))))
  )

;; http://www.flycheck.org
(use-package flycheck
  :ensure t
  :config
  (set-face-attribute 'flycheck-error nil :underline '(:color "#d32e00"))
  (set-face-attribute 'flycheck-warning nil :underline '(:color "#e3795c"))
  (set-face-attribute 'flycheck-info nil :underline '(:color "ForestGreen"))
  (global-flycheck-mode))

;; https://github.com/candid82/joker
;; brew install candid82/brew/joker
(use-package flycheck-joker
  :ensure t)

;; https://github.com/borkdude/clj-kondo
;; brew install borkdude/brew/clj-kondo
;; https://github.com/borkdude/flycheck-clj-kondo
(use-package flycheck-clj-kondo
  :ensure t)

;;(use-package clj-refactor
;;  :ensure t
;;  :pin melpa-stable
;;  :init
;;  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
;;  :config
;;  ;; Configure the Clojure Refactoring prefix:
;;  (cljr-add-keybindings-with-prefix "C-c ."))

(use-package parinfer
  :ensure t
  :bind (("C-," . parinfer-toggle-mode))
  :config
  (setq parinfer-extensions
        '(defaults       ; should be included.
           pretty-parens  ; different paren styles for different modes.
           paredit        ; Introduce some paredit commands.
           smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
           smart-yank     ; Yank behavior depend on mode.
           )))

(setq debug-on-error t)
(defun cljfmt ()
  (interactive)
  (when (or (eq major-mode 'clojure-mode)
            (eq major-mode 'clojurescript-mode))
    (with-current-buffer (buffer-name)
      (erase-buffer)
      (insert
       (shell-command-to-string (format "zprint < %s" buffer-file-name))))))

(require 'org)
(require 'ob-clojure)
(require 'cider)
(setq org-babel-clojure-backend 'cider)

;; [== kibit-mode ==]
;; (require 'kibit-mode)
;; (add-hook 'clojure-mode-hook 'kibit-mode)

;; ;; for kibit
;; (require 'compile)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;          '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
;; (add-to-list 'compilation-error-regexp-alist 'kibit)

;; (defun kibit ()
;;   "Run kibit on the current project.
;; Display the results in a hyperlinked *compilation* buffer."
;;   (interactive)
;;   (compile "lein kibit"))

;; (defun kibit-current-file ()
;;   "Run kibit on the current file.
;; Display the results in a hyperlinked *compilation* buffer."
;;   (interactive)
;;   (compile (concat "lein kibit " buffer-file-name)))
