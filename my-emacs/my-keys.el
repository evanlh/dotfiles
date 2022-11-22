(provide 'my-keys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INPUT MAPPING
;; hints: "S" = shift, "s" = command, "C" = control, "M" = alt
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "<s-left>") 'move-beginning-of-line)
(global-set-key (kbd "<s-right>") 'move-end-of-line)
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)
(global-set-key (kbd "<s-mouse-1>") 'mouse-major-mode-menu)

(setq shift-select-mode nil) ; shift-select mode
(setq delete-selection-mode t)  ; typing after selection kills the region

;; C-f C-f to fuzzy match on filename
(global-set-key (kbd "C-S-f") 'projectile-find-file)

;; make text increase/decrease apply to all buffers
;; new files don't get inited properly??
;; (defadvice text-scale-increase (around all-buffers (arg) activate)
;;   (dolist (buffer (buffer-list))
;;     (with-current-buffer buffer
;;       ad-do-it)))

;; move between windows
(progn (global-set-key (kbd "C-c <left>") 'windmove-left)
       (global-set-key (kbd "C-c <right>") 'windmove-right)
       (global-set-key (kbd "C-c <up>") 'windmove-up)
       (global-set-key (kbd "C-c <down>") 'windmove-down)

       ;; moving between windows, normalize with iTerm2 and (mod'd) tmux
       (global-set-key [M-s-left] 'windmove-left)
       (global-set-key [M-s-right] 'windmove-right)
       (global-set-key [M-s-up] 'windmove-up)
       (global-set-key [M-s-down] 'windmove-down)
       ;; same as above accounting for lack of arrow keys on ergodox
       (global-set-key (kbd "M-s-j") 'windmove-left)
       (global-set-key (kbd "M-s-l") 'windmove-right)
       (global-set-key (kbd "M-s-i") 'windmove-up)
       (global-set-key (kbd "M-s-k") 'windmove-down))

;; resize windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Smoother scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 1) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; fn+option+delete = kill word to the right in OS X inputs
;; (iterm2 ignores the option modifier)
(define-key global-map (kbd "<M-kp-delete>") 'paredit-forward-kill-word)

;; undo-tree-mode with aliases that match OS X undo/redo
(when (require 'undo-tree nil 'noerror)
 (global-undo-tree-mode 1)
 (defalias 'redo 'undo-tree-redo))

(global-set-key (kbd "s-z") 'undo) ; command+z
(global-set-key (kbd "s-Z") 'redo) ; shift+command+z

;; command-f, the default OSX search keybinding =>  forward search
(global-set-key (kbd "s-f") 'isearch-forward)
;; command-r, forward-replace
(global-set-key (kbd "s-r") 'query-replace-regexp)

;; substitute delete indentation with replace-regexp
(global-set-key (kbd "M-^") nil)
(global-set-key (kbd "M-^") 'replace-regexp)

;; OS X Lion fullscreen mode command-return
;; (global-set-key (kbd "<s-return>") 'ns-toggle-fullscreen)

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

;; enhanced completion library, same as inputrc binding
(global-set-key (kbd "M-/") 'hippie-expand)

;; turn off safety mode
(put 'downcase-region 'disabled nil)

;; delete selected text if you hit backspace or del
(delete-selection-mode 1)

;; close and kill buffer
(defun my-close-and-kill ()
  (interactive)
  (kill-buffer (buffer-name))
  (delete-window))
;; C-x 9 is harder to reach so I prefer to bind it to a combined close/kill
(global-set-key (kbd "C-x 9") 'my-close-and-kill)
;; one handed close window
(global-set-key (kbd "C-x c") 'delete-window)

;; search for thing at point
(defun my-search-at-point (begin end)
  (interactive)
  (thing-at-point 'string))

;; command-k compile shortcut
(define-key global-map (kbd "s-k") 'compile)

;; C-; to comment/un-comment, mnemonic is Lisp comment char
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; delete on backspace
(delete-selection-mode t)
