(provide 'my-darwin)
(if (eq system-type 'darwin)
    (progn
      ;; moving between windows, normalize with iTerm2 and (mod'd) tmux
      (progn (global-set-key [M-s-left] 'windmove-left)
             (global-set-key [M-s-right] 'windmove-right)
             (global-set-key [M-s-up] 'windmove-up)
             (global-set-key [M-s-down] 'windmove-down))
      (progn (global-set-key (kbd "C-c <left>") 'windmove-left)
             (global-set-key (kbd "C-c <right>") 'windmove-right)
             (global-set-key (kbd "C-c <up>") 'windmove-up)
             (global-set-key (kbd "C-c <down>") 'windmove-down))
      ;; (set-default-font "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-9-*-*-*-m-0-iso10646-1")
      ;; (set-face-attribute 'default nil :font "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")

      (set-default-font "-*-Source Code Pro-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
      ;; Mac OS X-style font-size control
      (define-key global-map (kbd "s-+") 'text-scale-increase)
	  (define-key global-map (kbd "s-=") 'text-scale-increase)
      (define-key global-map (kbd "s--") 'text-scale-decrease)

      ;; alt-click for mouse-2, command-click for mouse-3
      ;; broken?
      (setq mac-emulate-three-button-mouse t)

	  ;; use mac browser
	  (setq browse-url-browser-function 'browse-default-macosx-browser)

      ;; fix the PATH variable
      (defun set-exec-path-from-shell-PATH ()
        (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
          (setenv "PATH" path-from-shell)
          (setq exec-path (split-string path-from-shell path-separator))))

      (when window-system (set-exec-path-from-shell-PATH))
      ;; use OS X's Spotlight for M-x locate

      (setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s))))
  )
