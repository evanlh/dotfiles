;; The majority of this config is from Jack Rusher-- https://github.com/jackrusher/dotfiles
;; (Minor personal changes made by me -- @evanlh)
;;
;; I like my emacs to share as many behaviors as possible with OS
;; X and bash, to which end I've customized all three.

(require 'cl) ;; Common Lisp functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PATHS

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path "~/.emacs.d/additional/")
(add-to-list 'load-path "~/.emacs.d/my-emacs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PACKAGES

;; more (and more up-to-date) packages than plain ELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; this approached is taken from Prelude
(defvar evanlh-packages '(projectile helm-projectile icicles helm ac-slime auto-complete coffee-mode color-theme-sanityinc-tomorrow css-mode elisp-slime-nav expand-region find-file-in-project flycheck haml-mode idle-highlight-mode ido-ubiquitous inf-ruby js-comint yasnippet json-mode js2-mode js2-refactor magit markdown-mode molokai-theme paredit popup powerline ruby-block ruby-end ruby-mode slime slime-ritz smex  undo-tree yaml-mode tern tern-auto-complete ag))

(defun evanlh-packages-installed-p ()
  (loop for p in evanlh-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (evanlh-packages-installed-p)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p) evanlh-packages
    (when (not (package-installed-p p))
      (package-install p))))

(setq MY-JS-INDENT 4)
(require 'my-darwin)
(require 'my-windows)
(require 'my-linux)
(require 'my-keys)
(require 'my-appearance)
(require 'my-packages)

(global-auto-revert-mode t)

;; I'll be sending files from the command line
(server-start)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*", temporary-file-directory)))

