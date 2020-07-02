(use-package org
  ;; :ensure org-plus-contrib
  ;; :pin org
  :ensure t
  :pin melpa-stable
  :mode (("\\.org$" . org-mode))
  :bind
  (("C-c C-'" . org-edit-src-code)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))

  :config
  (setq org-export-with-sub-superscripts nil)
  (setq org-descriptive-links nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate
        '(lambda (lang body)
           (pcase lang
             ("emacs-lisp" nil)
             ("clojure" nil)
             (t t)))))

(use-package htmlize
  :ensure t)

(use-package org-preview-html
  :requires htmlize
  :ensure t)

(use-package ob-async
  ;; ref: https://github.com/astahlman/ob-async
  :requires (org ob)
  :ensure t)

;; ref: https://github.com/zweifisch/ob-http

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; ref: https://github.com/rlister/org-present
(use-package org-present
  :ensure t
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

;; emojify-mode,  emojify-insert-emoji
(use-package emojify
  :ensure t
  :init (global-emojify-mode -1)
  :config (if (display-graphic-p)
              (setq emojify-display-style 'image)
            (setq emojify-display-style 'ascii)))

(when (display-graphic-p)
  (use-package all-the-icons :ensure t))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package outshine
  :ensure t)

(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))

(defun daily ()
  (interactive)
  (let* ((daily-directory "~/Dropbox/org/")
         (daily-fname (format-time-string "%Y-%m-%d.org"))
         (daily-fpath (expand-file-name (concat daily-directory daily-fname))))
    (when (file-directory-p daily-directory)
      (find-file daily-fpath)
      (when (not (file-exists-p daily-fpath))
        (goto-char 0)
        (insert
         "#+TITLE: " (format-time-string "%Y-%m-%d\n")
         "#+AUTHOR: Eunpyoung Kim\n"
         "#+PROPERTY:header-args+ :comments both\n"
         "#+PROPERTY:header-args+ :results silent\n")
        (forward-line)))))

(use-package ob-translate
  ;; ref: https://github.com/krisajenkins/ob-translate
  ;; #+BEGIN_SRC translate
  ;; 안녕하세요
  ;; #+END_SRC

  ;; #+RESULTS:
  ;; : Hi
  :requires (org ob google-translate)
  :ensure t)

(use-package google-translate
  :ensure t)

(defun google-translate-auto ()
  "Automatically recognize and translate"
  ;; ref: https://github.com/minorugh/emacs.d/blob/master/inits/90_translate.el
  (interactive)
  (let ((lang-user "ko")
        (lang-target "en"))
    (if (region-active-p)
        (progn (setq mark-active nil)
               (if (string-match (format "\\`[%s]+\\'" "[:ascii:]") (buffer-substring (region-beginning) (region-end)))
                   (google-translate-translate lang-target lang-user
                                               (buffer-substring (region-beginning) (region-end)))
                 (google-translate-translate lang-user lang-target
                                             (buffer-substring (region-beginning) (region-end)))))
      (let ((string (read-string "Google Translate: ")))
        (if (string-match (format "\\`[%s]+\\'" "[:ascii:]") string)
            (google-translate-translate lang-target lang-user string)
          (google-translate-translate lang-user lang-target string))))))
