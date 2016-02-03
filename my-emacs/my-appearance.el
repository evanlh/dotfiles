(provide 'my-appearance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENCODING
;; always utf-8, all the time
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq slime-net-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; APPEARANCE
;; split new windows horizontal
;; (http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal)
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;; disable autofill mode
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(remove-hook 'js2-mode-hook #'turn-on-auto-fill)

; show menu
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
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

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
;; turn off the accursed auto-fill
(remove-hook 'prog-mode-hook 'esk-local-comment-auto-fill)

;; bring on the color theme
(setq custom-safe-themes)
(color-theme-sanityinc-tomorrow-bright)

;; powerline gives a much aesthetically improved mode line, the look
;; of which is stolen from vi.
(require 'powerline)
;;(powerline-default)

;; I hate the box on the mode-line
(set-face-attribute 'mode-line nil
                    :background "#000020"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :background "#000040")

;; tell me about my whitespace
(setq-default show-trailing-whitespace t)
(whitespace-mode)
;; but not during eshell sessions
(add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; four space tabs in general
(setq-default tab-width 4)
(setq c-basic-offset 4)

;; prog-mode-hook to hightlight XXX and BUG in code
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(XXX\\|BUG\\)" 1 font-lock-warning-face prepend)))))
