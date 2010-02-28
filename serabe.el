;; Function to add paths to clojure classpath.
;; Does it works?
(defun clojure-add-classpath (path)
  "Add a classpath to Clojure and refresh
slime-list-implementateions"
  (interactive "GPath: ")
  (push path swank-clojure-extra-classpaths)
  (setq slime-lisp-implementations
        (cons '(clojure ,(swank-clojure-cmd) :init swank-clojure-init)
              (remove-if #'(lambda (x) (eq (car x) 'clojure))
                         slime-lisp-implementations))))
