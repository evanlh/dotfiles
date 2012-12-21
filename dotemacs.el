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
(add-to-list 'load-path "~/.emacs.d/")

;; Make sure the path is set up for programs launched
;; via Spotlight, the Dock, Finder, &c, by running:
;; $ defaults write $HOME/.MacOSX/environment PATH "$PATH"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS INTEGRATION

;; use OS X's Spotlight for M-x locate
(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))

;; use MacSpell until ns-spell-checker support is ported to cocoa emacs
;; https://github.com/ruda/macspell
(setq ispell-program-name "~/bin/macspell.py")
(setq ispell-extra-args '("--encoding=utf8" "--auto-lang=yes"))

;; I'll be sending files from the command line
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENCODING

;; always utf-8, all the time
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq slime-net-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PACKAGES

;; more (and more up-to-date) packages than plain ELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; this approached is taken from Prelude
(defvar jackrusher-packages '(ack-and-a-half ac-slime auto-complete clojure-mode clojurescript-mode coffee-mode color-theme-sanityinc-tomorrow css-mode elisp-slime-nav expand-region find-file-in-project go-mode haml-mode haskell-mode idle-highlight-mode ido-ubiquitous inf-ruby js2-mode magit magithub markdown-mode molokai-theme paredit popup powerline ruby-block ruby-end ruby-mode slime slime-ritz smex starter-kit starter-kit-eshell starter-kit-js starter-kit-lisp starter-kit-ruby twilight-theme undo-tree yaml-mode ein))

(defun jackrusher-packages-installed-p ()
  (loop for p in jackrusher-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (jackrusher-packages-installed-p)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p jackrusher-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; APPEARANCE

;; split new windows horirzontal (http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal)
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;; show menu
(menu-bar-mode 1)
;; turn off splash screen messages
(setq inhibit-startup-echo-area-message t
      inhibit-startup-screen t)

;; drop window chrome
(setq tool-bar-mode -1
      fringe-mode -1)

;; speed up screen re-paint
(setq redisplay-dont-pause t)

;; typeface and spacing
(set-default-font "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(setq-default line-spacing 3)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Always display line & column numbers in mode-line
(setq line-number-mode t
      column-number-mode t)

;; disable bell on scroll (http://stackoverflow.com/questions/324457/disable-carbon-emacs-scroll-beep)
(defun my-bell-function ()
  (unless (memq this-command
    	'(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;; some general look and feel things
(setq font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; global line numbering
(setq linum-format " %5i ")
(global-linum-mode 0)

;; no highlight on the current line, nor word highlight on page
(remove-hook 'prog-mode-hook 'idle-highlight-mode)
(global-hl-line-mode -1)

;; bring on the color theme
(color-theme-sanityinc-tomorrow-night)

;; powerline gives a much aesthetically improved mode line, the look
;; of which is stolen from vi.
(require 'powerline)
(powerline-default)

;; I hate the box on the mode-line
(set-face-attribute 'mode-line nil
                    :background "#000020"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :background "#000040")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INPUT MAPPING

;;;; Normalize with Mac OS X
;; command + up/down/left/right = file start/end, line start/end

;; commenting command-up because I often use it
(global-set-key (kbd "<s-left>") 'move-beginning-of-line)
(global-set-key (kbd "<s-right>") 'move-end-of-line)
(global-set-key (kbd "<s-mouse-1>") 'mouse-major-mode-menu)

;; I prefer C-s-up and C-s-down so I don't do it accidentally
(global-set-key (kbd "<C-s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<C-s-down>") 'end-of-buffer)

(setq shift-select-mode t) ; shift-select mode
(delete-selection-mode 1)  ; typing after selection kills the region

;; Mac OS X-style font-size control
(define-key global-map (kbd "s-+") 'text-scale-increase)
(define-key global-map (kbd "s--") 'text-scale-decrease)

;; alt-click for mouse-2, command-click for mouse-3
;; broken?
(setq mac-emulate-three-button-mouse t)

;; Smoother scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 1) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; fn+option+delete = kill word to the right in OS X inputs
;; (iterm2 ignores the option modifier)
(define-key global-map (kbd "<M-kp-delete>") 'paredit-forward-kill-word)

;; undo-tree-mode with aliases that match OS X undo/redo
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "s-z") 'undo) ; command+z
(global-set-key (kbd "s-Z") 'redo) ; shift+command+z

;; command-f, the default OSX search keybinding =>  forward search
(global-set-key (kbd "s-f") 'isearch-forward)
;; command-r, forward-replace
(global-set-key (kbd "s-r") 'query-replace-regexp)

;; OS X Lion fullscreen mode command-return
(global-set-key (kbd "<s-return>") 'ns-toggle-fullscreen)

;;;; Normalize with the shell
;; make M-up and M-down the same as C-up and C-down
(global-set-key (kbd "<M-up>") 'backward-paragraph)
(global-set-key (kbd "<M-down>") 'forward-paragraph)

;; like in the shell
(global-set-key (kbd "C-d") 'delete-forward-char)

;; normalize with inputrc's C-w, use Mac-style command-X for cut region
(global-set-key (kbd "C-w") 'backward-kill-word)

;; add readline's backward-kill-line
(defun backward-kill-line ()
  "kill from point to the start of line"
  (interactive)
  (kill-line 0))
(global-set-key (kbd "C-x <C-backspace>") 'backward-kill-line)

;; prefer regexp in my backward search, inputrc-compatible binding
(global-set-key (kbd "^R") 'isearch-backward-regexp)

;; moving between windows, normalize with iTerm2 and (mod'd) tmux
(global-set-key [M-s-left] 'windmove-left)
(global-set-key [M-s-right] 'windmove-right)
(global-set-key [M-s-up] 'windmove-up)
(global-set-key [M-s-down] 'windmove-down)

;; enhanced completion library, same as inputrc binding
(global-set-key (kbd "M-/") 'hippie-expand)

;; expand-region is super handy while editing code
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; turn off safety mode
(put 'downcase-region 'disabled nil)

;; delete selected text if you hit backspace or del
(delete-selection-mode t)

;; ack-and-a-half
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
(global-set-key (kbd "C-a") 'ack)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROGRAMMING/LANGUAGES

;; four space tabs in general
(setq-default tab-width 4)
(setq c-basic-offset 4)

;; auto-complete-mode - popup help
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-use-quick-help t)
(setq ac-auto-show-menu 0.)
(setq ac-quick-help-delay 0.3)
(ac-config-default)
(ac-flyspell-workaround)
(define-key ac-complete-mode-map [tab] 'ac-expand)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;; TODO customize ac-complete for color theme
;(set-face-background 'ac-candidate-face "#366060")
;(set-face-foreground 'ac-selection-face "#1f1f1f")
;(set-face-background 'ac-selection-face "#8cd0d3")
;(set-face-foreground 'ac-selection-face "#1f1f1f")

;; command-k compile shortcut
(define-key global-map (kbd "s-k") 'compile)

;; C-; to comment/un-comment, mnemonic is Lisp comment char
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; prog-mode-hook to hightlight XXX and BUG in code
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(XXX\\|BUG\\)" 1 font-lock-warning-face prepend)))))

;; tell me about my whitespace, clean it up on save
(setq-default show-trailing-whitespace t)
(whitespace-mode)
(add-hook 'before-save-hook
          'whitespace-cleanup
          nil t)

;; but not during eshell sessions
(add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;;two space tabs in coffee
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; adjust paredit's key bindings so they don't override
;; my preferred navigation keys, plus add some brace
;; matching sugar across all modes
(eval-after-load 'paredit
  '(progn
     ;; fights with my preferred navigation keys
     (dolist (binding (list (kbd "M-<up>") (kbd "M-<down>") (kbd "C-M-<left>") (kbd "C-M-<right>")))
       (define-key paredit-mode-map binding nil))

     ;; not just in lisp mode(s)
     (global-set-key (kbd "C-M-<left>") 'backward-sexp)
     (global-set-key (kbd "C-M-<right>") 'forward-sexp)

     (global-set-key (kbd "M-(") 'paredit-wrap-round)
     (global-set-key (kbd "M-[") 'paredit-wrap-square)
     (global-set-key (kbd "M-{") 'paredit-wrap-curly)

     (global-set-key (kbd "M-)") 'paredit-close-round-and-newline)
     (global-set-key (kbd "M-]") 'paredit-close-square-and-newline)
     (global-set-key (kbd "M-}") 'paredit-close-curly-and-newline)))

;;;; geiser for racket, why is this not in the package manager?
;; currently wrapped in this function because forkers of this
;; config won't have geiser installed by default
(defun configure-racket ()
  (add-to-list 'load-path (concat dotfiles-dir "vendor/geiser-0.1.4/elisp"))
  (require 'geiser)
  (setq geiser-active-implementations '(racket)))

;;;; clojure-mode uses lein repl
(add-hook 'clojure-mode-hook
          (lambda () (setq inferior-lisp-program "lein repl")))

;; XXX temporarily commented out because it fights with clojure
;; (*really* irritating)
;; (setq slime-lisp-implementations
;;       `((clojure ,(swank-clojure-cmd) :init swank-clojure-init)
;;         (sbcl ("sbcl") :coding-system utf-8-unix)))
;;;; sbcl with quicklisp under slime
;; (add-hook 'lisp-mode-hook
;;           (lambda () (setq inferior-lisp-program "sbcl --noinform")))
;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq common-lisp-hyperspec-root "file:/Users/jack/lisp/HyperSpec/")

;; add auto-completion for slime
(add-hook 'slime-mode-hook 'set-up-slime-ac)
;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;; (eval-after-load "auto-complete"
;;    '(add-to-list 'ac-modes 'slime-repl-mode))

;; no need to highlight trailing whitepsace in the repl
(add-hook 'slime-repl-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (require 'clojure-mode)
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))

;;;; Haskell preferences
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROSE AND NOTES

(autoload 'markdown-mode "markdown-mode" "Mode for editing Markdown documents" t)
(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)$" . markdown-mode) auto-mode-alist))

;;; DO NOT TOUCH

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
