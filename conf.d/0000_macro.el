(setq true t)
(setq false nil)
(setq windows? (eq system-type 'windows-nt))
(setq mac? (eq system-type 'darwin))

(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(defmacro -> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

(defmacro comment (&rest body)
  nil)
