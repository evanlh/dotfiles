(provide 'callstack)
;; (defvar callstack-string-at-point nil)
;; (defun callstack-interactive ()
;;   (interactive)
;;   ;; (message "%s" (buffer-substring-no-properties begin end))
;;   (setq callstack-string-at-point (current-word))
;;   (message "%s" callstack-string-at-point)
;;   )

;; (global-set-key (kbd "C-<") 'callstack-interactive)

;; (thing-at-point 'word)
;; (interactive "r")
;; current-prefix-arg
;; (call-interactively 'callstack-interactive)

(defvar cl-current-org-buffer nil)
(defvar cl-code-to-copy nil)

(defun cl-set-current-org-buffer ()
  (interactive)
  (setq cl-current-org-buffer (current-buffer))
  (message (concat "Selected " buffer-file-truename " as target org-buffer")))

(defun cl-go-to-end-of-file-section (file)
  ;; TODO search for the filename LINK prefixed with ** and go to end of buffer
  ;; or the next ** whichever comes first. if no filename LINK is found
  ;; go to end of buffer and create it
  (goto-char (point-max)))

(defun cl-copy-region-to-org-code-block (point mark)
  (interactive "r")
  ;; (message "%s %s" point mark)
  ;; make sure the current org buffer is set
  (if cl-current-org-buffer
      (let ((file buffer-file-truename)
          (oldbuf (current-buffer))
          (length (- mark point))
          (linenum (line-number-at-pos point)))
      ;; XXX store the org link
      ;; (call-interactively 'org-store-link)
        (save-excursion
        ;; select the current org buffer
        (set-buffer cl-current-org-buffer)
        ;; find the section in the org buffer with that file. if none exists add a section.
        (cl-go-to-end-of-file-section file)
        (setq cl-start-code)
        ;; insert a newline at end of section
        (newline)
        (newline)
        ;; XXX having issues with interactive version insert the org-babel link
        ;; (call-interactively 'org-insert-link)
        (insert (org-make-link-string
                 (concat "file:" file "::" (number-to-string linenum))
                 (concat file "::" (number-to-string linenum))))
        (newline)
        ;; TODO give the region in the org buffer   org-babel-demarcate-block
        (insert "#+BEGIN_SRC js")
        (newline)
        ;; paste the region into the current org
        (insert-buffer-substring oldbuf point mark)
        (newline)
        (insert "#+END_SRC")
        (newline)
        ))

    (message "Please select the target org-mode buffer first")
    ))

(global-set-key (kbd "C-c C-.") 'cl-set-current-org-buffer)
(global-set-key (kbd "ESC .") 'cl-set-current-org-buffer)
(global-set-key (kbd "ESC >") 'cl-copy-region-to-org-code-block)

