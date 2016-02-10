(defvar callstack-string-at-point nil)
(defun callstack-interactive ()
  (interactive)
  ;; (message "%s" (buffer-substring-no-properties begin end))
  (setq callstack-string-at-point (current-word))
  (message "%s" callstack-string-at-point)
  )

(global-set-key (kbd "C-<") 'callstack-interactive)

(thing-at-point 'word)
(interactive "r")
current-prefix-arg

(call-interactively 'callstack-interactive)
