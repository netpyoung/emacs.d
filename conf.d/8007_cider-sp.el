;; NOTE(pyoung):
;; I know `cider-eval-defun-to-comment` also provide similar feature.
;; but that function only support for `value,` not `out` and `err`.
(require 'cider)

(defun cider-sp:format (prefix str)
  ;; (xx:format "@" "1\n2\n") => "@1\n@2\n"
  (let* ((str (replace-regexp-in-string "\^M" "" str))
         (str (split-string str "\n" t))
         (line-prefix (concat "\n" prefix)))
    (concat prefix (mapconcat 'identity  str line-prefix) "\n")))


(defun cider-sp:print-handler (buffer)
  ;; buffer value-handler stdout-handler stderr-handler &optional done-handler
  (nrepl-make-response-handler
   buffer
   (lambda (buffer value)
     (with-current-buffer buffer (insert (cider-sp:format ";;=> " value))))
   (lambda (buffer out)
     (with-current-buffer buffer (insert (cider-sp:format ";;>> " out))))
   (lambda (buffer err)
     (let ((err (first (split-string err "  "))))
       (with-current-buffer buffer (insert (cider-sp:format ";;-> " err)))))
   '()))


(defun cider-sp:eval-last-exp ()
  (interactive)
  (let* ((region (cider-defun-at-point 'bounds))
         (expr (buffer-substring (car region) (car (cdr region)))))
    (goto-char (car (cdr region)))
    (cider-nrepl-request:eval
     expr
     (cider-sp:print-handler (current-buffer))
     (cider-current-ns))))

;; [== key setting ==]
(define-key cider-mode-map [(control return)] 'cider-sp:eval-last-exp)
