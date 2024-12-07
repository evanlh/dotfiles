;; -*- emacs-lisp -*-
;;
;; The majority of this config is from Jack Rusher-- https://github.com/jackrusher/dotfiles
;; (Minor personal changes made by me -- @evanlh)
;;
;; I like my emacs to share as many behaviors as possible with OS
;; X and bash, to which end I've customized all three.

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

(defun is-home-machine () (string= (system-name) "Evans-MacBook-Pro.local"))
(defun is-work-machine () (string= (system-name) "LV344H1G32"))

(if (is-home-machine)
   ;; home machine
   (progn
     (setq MY-JS-INDENT 2)
     (setq backup-directory-alist `(("*" .  "/Users/elh/backups")))
     (set-frame-font "-*-Source Code Pro-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
     ))
(if (is-work-machine)
 ;; work machine
 (progn
   (set-frame-font "-*-Source Code Pro-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
   (setq MY-JS-INDENT 4)
   (setq backup-directory-alist `(("*" .  "/Users/elawrencehur/backups")))
   (setq auto-save-file-name-transforms `(("*", "/Users/elawrencehur/backups" t)))
   (setq url-proxy-services
         '(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ("http" . "proxy.inet.bloomberg.com:81")
          ("https" . "proxy.inet.bloomberg.com:81")))
   )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PACKAGES

;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (setq package-check-signature nil)
;; (setq gnutls-log-level 5)
;; (setq gnutls-verify-error t)
;; (setq gnutls-trustfiles '("/usr/local/etc/libressl/cert.pem"))
;; (setq network-security-protocol-checks nil)
;; (gnutls-macs)
;; (gnutls-ciphers)

(setq package-archives '())
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;; 	         '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)


(setq packages-emacs29 '(idle-highlight-mode tree-sitter-langs))
(setq packages-emacs28 '(eglot))
;; this approached is taken from Prelude
(setq evanlh-packages '(ag undo-tree projectile helm-projectile helm ac-slime auto-complete clojure-mode coffee-mode color-theme-sanityinc-tomorrow css-mode elisp-slime-nav expand-region find-file-in-project go-mode haml-mode haskell-mode ido-completing-read+ inf-ruby js2-mode js2-refactor magit markdown-mode molokai-theme paredit popup powerline rainbow-delimiters restclient ruby-end ruby-mode skewer-mode slime smex js-comint twilight-theme undo-tree yaml-mode ein cider tern tern-auto-complete jedi flycheck ocp-indent merlin elpy ox-hugo ox-gfm rust-mode))

(cond ((eq emacs-major-version 28)
       (setq packages-to-install
             (append evanlh-packages packages-emacs28)))
      ((eq emacs-major-version 29)
       (setq packages-to-install
             (append evanlh-packages packages-emacs29)))
      (t (setq packages-to-install evanlh-packages)))

(defun evanlh-packages-installed-p ()
  (cl-loop for p in packages-to-install
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (evanlh-packages-installed-p)
 (message "%s" "Emacs is now refreshing its package database...")
 (package-refresh-contents)
 (message "%s" " done.")

  ;;install the missing packages
 (dolist (p packages-to-install)
   (when (not (package-installed-p p))
     (package-install p))))

(setq MY-ORG-DIRECTORY "~/writing")

(require 'my-keys)
(require 'my-appearance)
(require 'my-packages)
(require 'my-darwin)
(require 'my-windows)
(require 'my-linux)

;; (require 'init-bde-style)

;;temporary-file-directory
;; I'll be sending files from the command line
(server-start)

(setq auto-save-interval 20)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-cljs-lein-repl
   "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
 '(custom-enabled-themes '(sanityinc-tomorrow-bright))
 '(custom-safe-themes
   '("6fc9e40b4375d9d8d0d9521505849ab4d04220ed470db0b78b700230da0a86c1" "b11edd2e0f97a0a7d5e66a9b82091b44431401ac394478beb44389cf54e6db28" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default))
 '(package-selected-packages
   '(tree-sitter-langs tree-sitter typescript-ts-mode js-ts-mode eglot tern-auto-complete tern epresent dyalog-mode gnu-apl-mode undo-tree typescript-mode org-capture-pop-frame org-roam yafolding notmuch ox-tufte ox-hugo ox-gfm rainbow-delimiters ein markdown-mode js2-mode ido-completing-read+ clojure-mode projectile "cider" ox-rst groovy-mode tide pep8 flycheck-pyflakes py-autopep8 elpy jedi-direx popwin rust-mode impatient-mode irony company-tern company-lsp lsp-javascript-typescript lsp-mode yaml-mode utop twilight-theme tuareg smex slime-ritz skewer-mode sicp ruby-end ruby-block restclient rect-mark powerline paredit org-jira ocp-indent molokai-theme merlin markdown-mode+ magit jsx-mode json-mode js2-refactor js-comint jedi inf-ruby ido-ubiquitous idle-highlight-mode icicles helm-projectile haskell-mode haml-mode go-mode geiser flycheck find-file-in-project expand-region exec-path-from-shell evil elisp-slime-nav ein-mumamo color-theme-sanityinc-tomorrow coffee-mode cider aggressive-fill-paragraph ag ac-slime))
 '(safe-local-variable-values '((Base . 10) (Package . LAPIDARY) (Syntax . Common-Lisp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-drawer ((t (:foreground "LightSkyBlue" :height 0.6))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.3)))))
(put 'upcase-region 'disabled nil)
