(provide 'my-darwin)
(if (eq system-type 'darwin)
    (progn
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
