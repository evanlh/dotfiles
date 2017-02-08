;; -*- emacs-lisp -*-
;;
;; The majority of this config is from Jack Rusher-- https://github.com/jackrusher/dotfiles
;; (Minor personal changes made by me -- @evanlh)
;;
;; I like my emacs to share as many behaviors as possible with OS
;; X and bash, to which end I've customized all three.

(require 'cl) ;; Common Lisp functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PATHS

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path "~/.emacs.d/additional")
(add-to-list 'load-path "~/.emacs.d/my-emacs")
;; makes zsh work properly
;; contents of ~/bin/shell:
;; #!/bin/sh
;; TERM=emacs exec zsh
(setenv "ESHELL" (expand-file-name "~/bin/eshell"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PACKAGES

;; more (and more up-to-date) packages than plain ELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

;; this approached is taken from Prelude
(defvar evanlh-packages '(projectile dired+ helm-projectile icicles helm ac-slime auto-complete clojure-mode coffee-mode color-theme-sanityinc-tomorrow css-mode elisp-slime-nav expand-region find-file-in-project go-mode haml-mode haskell-mode idle-highlight-mode ido-ubiquitous inf-ruby js2-mode js2-refactor magit markdown-mode molokai-theme paredit popup powerline restclient ruby-block ruby-end ruby-mode skewer-mode slime slime-ritz smex js-comint twilight-theme undo-tree yaml-mode ein cider tern tern-auto-complete jedi flycheck utop ocp-indent merlin))

(defun evanlh-packages-installed-p ()
  (loop for p in evanlh-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (evanlh-packages-installed-p)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")

  ;; install the missing packages
  (dolist (p evanlh-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(setq MY-JS-INDENT 4)
(setq MY-ORG-DIRECTORY "~/writing")
(require 'my-darwin)
(require 'my-windows)
(require 'my-linux)
(require 'my-keys)
(require 'my-appearance)
(require 'my-packages)


;; I'll be sending files from the command line
(server-start)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*", temporary-file-directory)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-cljs-lein-repl "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
