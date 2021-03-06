;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq vendorfiles-dir (concat dotfiles-dir "/vendor"))

;; Load up ELPA, the package manager

(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/jabber"))
(add-to-list 'load-path vendorfiles-dir)
(add-to-list 'load-path (concat vendorfiles-dir "/coffee-mode"))
(add-to-list 'load-path (concat vendorfiles-dir "/color-theme"))
(add-to-list 'load-path (concat vendorfiles-dir "/cucumber"))
(add-to-list 'load-path (concat vendorfiles-dir "/drag-stuff"))
(add-to-list 'load-path (concat vendorfiles-dir "/erc"))
(add-to-list 'load-path (concat vendorfiles-dir "/gist"))
(add-to-list 'load-path (concat vendorfiles-dir "/git-emacs"))
(add-to-list 'load-path (concat vendorfiles-dir "/go-mode"))
(add-to-list 'load-path (concat vendorfiles-dir "/google-maps"))
(add-to-list 'load-path (concat vendorfiles-dir "/groovy-mode"))
(add-to-list 'load-path (concat vendorfiles-dir "/ido-mode"))
(add-to-list 'load-path (concat vendorfiles-dir "/mode-compile"))
(add-to-list 'load-path (concat vendorfiles-dir "/occam-mode"))
(add-to-list 'load-path (concat vendorfiles-dir "/rhtml-mode"))
(add-to-list 'load-path (concat vendorfiles-dir "/rspec-mode"))
(add-to-list 'load-path (concat vendorfiles-dir "/undo-tree"))
(add-to-list 'load-path (concat vendorfiles-dir "/yasnippet"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(setq serabe-file (concat dotfiles-dir "serabe.el"))

(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(require 'starter-kit-elpa)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
(require 'starter-kit-perl)
(require 'starter-kit-ruby)
(require 'starter-kit-js)

(regen-autoloads)
(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))
(if (file-exists-p user-specific-config) (load user-specific-config))

;; coffee-mode
(require 'coffee-mode)
;(add-to-list 'auto-mode-list '("\\.coffee$" . coffee-mode))
;(add-to-list 'auto-mode-list '("Cakefile" . coffee-mode))
;(defun coffee-custom ()
;  "coffee-mode-hook"
;  (set (make-local-variable 'tab-width) 2))

;(add-hook coffee-mode-hook
;          '(lambda () (coffee-custom)))

;; Go-mode
(require 'go-mode-load)

;; Gist
(require 'gist)

;; git-emacs
(require 'git-emacs)
(setq git-state-modeline-decoration 'git-state-decoration-large-dot)
(require 'git-emacs-autoloads)

;; Occam-mode
(require 'occam-mode-load)

;; Go-mode
(require 'go-mode-load)

;; Occam-mode
(require 'occam-mode-load)

;; Color Theme
(require 'color-theme)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

;; Yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat vendorfiles-dir "/yasnippet/snippets"))

;; ERC
(require 'erc)

;; dot-mode
(load-file (concat vendorfiles-dir "/dot-mode/graphviz-dot-mode.el"))


;; drag-stuff mode
(require 'drag-stuff)

;; Mode-compile
(autoload 'mode-compile "mode-compile"
  "Mode to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)

(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;; IDO
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; Groovy
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode" t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
;; for easyb specification files
(add-to-list 'auto-mode-alist '("\.specification$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; google mode
(require 'google-maps)

;; rvm
(require 'rvm)
(rvm-use-default)

;; rhtml
(require 'rhtml-mode)

;; My own .el file
(load serabe-file)

;; rspec
(require 'rspec-mode)

;; cucumber
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; My own .el file
(load serabe-file)

;;; init.el ends here
