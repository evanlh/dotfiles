(provide 'my-windows)
(if (or (equal system-type 'windows-nt) (equal system-type 'cygwin))
    (progn
      ;; keys for windows
      (setq w32-pass-lwindow-to-system nil
            w32-pass-rwindow-to-system nil
            w32-pass-apps-to-system nil
            w32-lwindow-modifier 'super ; Left Windows key
            w32-rwindow-modifier 'super ; Right Windows key
            w32-apps-modifier 'hyper)

      (cua-mode t)

      (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
      (add-to-list 'exec-path "C:/cygwin/bin")
      (add-to-list 'exec-path "C:/cygwin/usr/bin")
      (add-to-list 'exec-path "C:/cygwin/usr/local/bin")
      (setq shell-file-name "bash")
      ;; (setq explicit-sh.exe-args '("--login" "-i"))
      ;; (setq explicit-shell-file-name shell-file-name)

      ;; Mouse wheel behavior
      (global-set-key [C-wheel-up] 'text-scale-increase)
      (global-set-key [C-wheel-down] 'text-scale-decrease)
      (global-set-key [C-down-mouse-2] 'text-scale-normal-size)

      ;; font - to get this on w32 type (w32-select-font) followed by
      ;; M-x eval-print-last-sexp
      ;;(set-face-attribute 'default nil :font "DejaVu Sans Mono-11")
      (set-face-attribute 'default nil :font "Source Code Pro Semibold-11")

      (setq mouse-drag-copy-region nil)
      ;; when pasting with linux x11 middle click, set to paste at cursor position, not at click position
      (setq mouse-yank-at-point t)
      (cua-mode t)
      (global-set-key (kbd "<M-up>") 'beginning-of-buffer)
      (global-set-key (kbd "<M-down>") 'end-of-buffer)

      (add-to-list 'load-path "~/lintnode")
      ;; (require 'flymake-jslint)
      ;; Make sure we can find the lintnode executable
      (setq lintnode-location "~/lintnode")
      ;; JSLint can be... opinionated
      (setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))

      ;; try to imrpove slowness
      (setq w32-get-true-file-attributes nil)

      ;; js indent level
      (defvar evanlh-js-indent-level 4)
      ;; (setq inferior-js-program-command "node")
      ;; (setq inferior-js-mode-hook
      ;;       (lambda ()
      ;;         ;; We like nice colors
      ;;         (ansi-color-for-comint-mode-on)
      ;;         ;; Deal with some prompt nonsense
      ;;         (add-to-list 'comint-preoutput-filter-functions
      ;;                      (lambda (output)
      ;;                        (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
      ;;                                                  (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))
      ;;         ))
      ))
