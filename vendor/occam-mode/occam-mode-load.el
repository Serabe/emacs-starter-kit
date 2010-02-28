(autoload 'occam-mode "occam-mode" "Major mode for Occam" t nil)

(setq auto-mode-alist (cons '("\\.occ$" . occam-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.inc$" . occam-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pgm$" . occam-mode) auto-mode-alist))


;;;***
(provide 'occam-mode-load)
