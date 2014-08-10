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

(setq exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin")))
(setenv "PATH" (mapconcat 'identity exec-path ":"))

;; Make sure the path is set up for programs launched
;; via Spotlight, the Dock, Finder, &c, by running:
;; $ defaults write $HOME/.MacOSX/environment PATH "$PATH"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS INTEGRATION

;; use OS X's Spotlight for M-x locate
(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))

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
(defvar evanlh-packages '(projectile dired+ helm-projectile icicles helm ack-and-a-half ac-slime auto-complete clojure-mode coffee-mode color-theme-sanityinc-tomorrow css-mode elisp-slime-nav expand-region find-file-in-project go-mode haml-mode haskell-mode idle-highlight-mode ido-ubiquitous inf-ruby js2-mode js2-refactor magit markdown-mode molokai-theme paredit popup powerline ruby-block ruby-end ruby-mode skewer-mode slime slime-ritz smex starter-kit starter-kit-eshell starter-kit-js starter-kit-lisp starter-kit-ruby twilight-theme undo-tree web-mode yaml-mode ein cider))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; APPEARANCE
(desktop-save-mode 1)
(cua-mode t)
;; split new windows horizontal
;; (http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal)
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
(if (string-equal system-type "darwin")
    (progn (set-default-font "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
           (set-face-attribute 'default nil :font "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")))

(setq-default line-spacing 3)


;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Always display line & column numbers in mode-line
(setq line-number-mode t
      column-number-mode t)

;; disable bell on scroll
;; http://stackoverflow.com/questions/324457/disable-carbon-emacs-scroll-beep
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
(if (string-equal system-type "darwin")
  (color-theme-sanityinc-tomorrow-night)
  (color-theme-sanityinc-tomorrow-bright))


;; powerline gives a much aesthetically improved mode line, the look
;; of which is stolen from vi.
(require 'powerline)
(powerline-default-theme)
;;(powerline-default)

;; I hate the box on the mode-line
(set-face-attribute 'mode-line nil
                    :background "#000020"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :background "#000040")

(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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

;; (setq shift-select-mode t) ; shift-select mode
(setq shift-select-mode nil)
(setq delete-selection-mode 1)  ; typing after selection kills the region

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
(global-set-key (kbd "<M-right>") 'end-of-line)
(global-set-key (kbd "<M-left>") 'beginning-of-line)

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
system-type
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
;; hook AC into completion-at-point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; ac-mode for nrepl for clojure
;; (require 'ac-nrepl)
;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))

;; (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
;; (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

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

;; this is needed to prevent ac-nrepl from breaking
;; clojure-mode's starting of nrepl-interaction mode
;; on nrepl-jack-in
;; (setq nrepl-connected-hook (reverse nrepl-connected-hook))

;; disable stack traces outside of repl
;;(setq nrepl-popup-stacktraces nil)
;; rainbow parens
;;(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)
;; hide special buffers
;;(setq nrepl-hide-special-buffers t)

;; add auto-completion for slime
(add-hook 'slime-mode-hook 'set-up-slime-ac)
;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;; (eval-after-load "auto-complete"
;;    '(add-to-list 'ac-modes 'slime-repl-mode))

;; no need to highlight trailing whitepsace in the repl
(add-hook 'slime-repl-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; js2-mode
(require 'js2-mode)
(setq-default js2-auto-indent-p t)
(setq-default js2-global-externs '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'javascript-mode-hook
          (lambda () (flymake-mode t)))
(add-hook 'js2-mode-hook
          (lambda ()
            (setq js2-basic-offset 2)
            (setq js2-bounce-indent-p t)))

(setq js2-basic-offset 2)
(setq js-indent-level 2)
(setq c-basic-offset 2)
(setq indent-tabs-mode nil)

;; skewer mode for browser mind control
(require 'skewer-mode)
(require 'skewer-repl)
(require 'skewer-html)
(require 'skewer-css)
(defun skewer-start ()
  (interactive)
  (let ((httpd-port 8023))
    (httpd-start)
    (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROSE AND NOTES

(autoload 'markdown-mode "markdown-mode" "Mode for editing Markdown documents" t)
(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)$" . markdown-mode) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODULES I DEPEND ON

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ack-and-a-half
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
(setq ack-and-a-half-prompt-for-directory 1)
(global-set-key (kbd "C-x C-a") 'ack)

;; Icicles (got sick of em)
(icy-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; web-mode
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))

;; (add-to-list 'auto-mode-alist '("\\.php" . php-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Projectile
(projectile-global-mode t)
(setq projectile-completion-system 'ido)
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "C-c t") 'helm-cmd-t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EIN
(setq ein:use-auto-complete t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; php-mode

;; from http://www.emacswiki.org/emacs/PhpMode
(add-hook 'php-mode-hook (lambda ()
    (defun ywb-php-lineup-arglist-intro (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (+ (current-column) c-basic-offset))))
    (defun ywb-php-lineup-arglist-close (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (current-column))))
    (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
    (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-startup-indented 1)
(setq org-support-shift-select 1)
(setq org-pretty-entities 1)
(setq org-todo-keywords
       '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "DEFERRED")))

(setq org-directory "~/writing")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(add-hook 'org-mode-hook 'visual-line-mode)
;; soft wrap lines
;;(add-hook 'org-mode-hook 'soft-wrap-lines)
;; (defun soft-wrap-lines ()
;;   "Make lines wrap at window edge and on word boundary, in current buffer."
;;   (interactive)
;;   (setq truncate-lines nil)
;;   (setq word-wrap t))

;; capture templates
(setq org-capture-templates
  '(("t" "Todo" entry (file+headline "~/writing/dailytodo.org" "UNASSIGNED")
         "** TODO %?\n  %i\n")
    ("d" "Draft" entry (file+datetree "~/writing/drafts.org")
         "* Entered on %U\n  %i\n")))
;; agenda files
(setq org-agenda-files (quote ("~/writing/entropy.org" "~/writing/ideas.org" "~/writing/projects.org" "~/writing/dailytodo.org")))

;; long lines mode instead
(add-hook 'org-mode-hook 'longlines-mode)

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))
; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)

;; mobileorg
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/writing/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Misc

(setq ffip-patterns '("*.html" "*.org" "*.md" "*.el" "*.clj" "*.py" "*.rb" "*.js" "*.pl" "*.sh" "*.erl" "*.hs" "*.ml" "*.php" "*.html" "*.phtml"))
(setq ffip-limit 4096)
(toggle-diredp-find-file-reuse-dir 1)
(global-auto-revert-mode)
(global-set-key (kbd "C-x f") 'find-file-in-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ido
;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)
(setq ido-enable-flex-matching t)

;;;;;;;;;;;;;;; DO NOT TOUCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#000000"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin")))
 '(fci-rule-color "#2a2a2a")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#d54e53") (40 . "#e78c45") (60 . "#e7c547") (80 . "#b9ca4a") (100 . "#70c0b1") (120 . "#7aa6da") (140 . "#c397d8") (160 . "#d54e53") (180 . "#e78c45") (200 . "#e7c547") (220 . "#b9ca4a") (240 . "#70c0b1") (260 . "#7aa6da") (280 . "#c397d8") (300 . "#d54e53") (320 . "#e78c45") (340 . "#e7c547") (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


