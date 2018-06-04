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
(add-to-list  'exec-path "/usr/local/bin")

(setq gc-cons-threshold 8000000)
;;(setq garbage-collection-messages t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PACKAGES

;; more (and more up-to-date) packages than plain ELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; this approached is taken from Prelude
;; (defvar evanlh-packages '(projectile helm-projectile icicles helm ac-slime  coffee-mode color-theme-sanityinc-tomorrow css-mode elisp-slime-nav expand-region find-file-in-project flycheck haml-mode idle-highlight-mode ido-ubiquitous inf-ruby js-comint yasnippet json-mode js2-mode js2-refactor magit markdown-mode molokai-theme paredit popup powerline ruby-block ruby-end ruby-mode slime slime-ritz  undo-tree yaml-mode tern ag))
(setq evanlh-packages '(projectile color-theme-sanityinc-tomorrow find-file-in-project ido-ubiquitous smex org json-mode js2-mode js2-refactor magit paredit popup powerline undo-tree ag toolkit-tramp))

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

;;(require 'tramp)
;;(require 'toolkit-tramp)
;; hoping to fix random windows freezes http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; (defun my-minibuffer-setup-hook ()
;;   (setq gc-cons-threshold most-positive-fixnum))

;; (defun my-minibuffer-exit-hook ()
;;   (setq gc-cons-threshold 800000))

;; (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; (global-auto-revert-mode t)

;; I'll be sending files from the command line
(server-start)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*", temporary-file-directory)))


(setq gc-cons-threshold 4000)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
