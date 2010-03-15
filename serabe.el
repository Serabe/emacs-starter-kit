;; AUC TEX con RefTEX

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq TeX-default-mode '"LaTeX-mode")

;; ERC Config
(defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    add-to-it))

(setq erc-keywords '("serabe" "Serabe" "serabe_"))

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; ERC Libnotify
(defun clean-message (s)
  (setq s (replace-regex-in-string "'" "&apos;"
          (replace-regex-in-string "\"" "&quot;"
          (replace-regex-in-string "&" "&amp;"
          (replace-regex-in-string "<" "&lt;"
          (replace-regex-in-string ">" "&gt;" s)))))))

(defun call-libnotify (matched-type nick msg)
  (let* ((cmsg (split-string (clean-message msg)))
         (nick (first (split-string nick "!")))
         (msg (mapconcat 'identity (rest cmsg) " ")))
    (shell-command-to-string
     (format "notify-send -u critical '%s says:' '%s'" nick msg))))

(add-hook 'erc-text-matched-hook 'call-libnotify)

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
