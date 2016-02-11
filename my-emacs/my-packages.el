(provide 'my-packages)

;; expand-region is super handy while editing code
(when (require 'expand-region nil 'noerror)
  (global-set-key (kbd "C-=") 'er/expand-region))

;; rectangular selection
(when (require 'rect-mark nil 'noerror)
  (global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
  (global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
  (global-set-key (kbd "C-x r C-w") 'rm-kill-region)
  (global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)
  (autoload 'rm-set-mark "rect-mark"
    "Set mark for rectangle." t)
  (autoload 'rm-exchange-point-and-mark "rect-mark"
    "Exchange point and mark for rectangle." t)
  (autoload 'rm-kill-region "rect-mark"
    "Kill a rectangular region and save it in the kill ring." t)
  (autoload 'rm-kill-ring-save "rect-mark"
    "Copy a rectangular region to the kill ring." t)
  )


;; PAREDIT
(when (require 'paredit nil 'noerror)
  ;; adjust paredit's key bindings so they don't override
  ;; my preferred navigation keys, plus add some brace
  ;; matching sugar across all modes
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
  (global-set-key (kbd "M-}") 'paredit-close-curly-and-newline)
  )

;; autcomplete-mode
;; (when (require 'auto-complete nil 'noerror)
;;   (require 'auto-complete-config)
;;   (global-auto-complete-mode t)
;;   (setq ac-use-quick-help t)
;;   (setq ac-auto-show-menu 0.5)
;;   (setq ac-quick-help-delay 0.5)
;;   (ac-config-default)
;;   (ac-flyspell-workaround)
;;   (define-key ac-complete-mode-map [tab] 'ac-expand)
;;   (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;;   ;; hook AC into completion-at-point
;;   (defun set-auto-complete-as-completion-at-point-function ()
;;     (setq completion-at-point-functions '(auto-complete)))
;;   (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;   )

;; ac-nrepl for clojure
(when (require 'ac-nrepl nil 'noerror)
  (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
  (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'nrepl-mode))

  (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode) 
  (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))


;; markdown-mode
(when (require 'markdown-mode nil 'noerror)
  (setq auto-mode-alist
        (cons '("\\.\\(md\\|markdown\\)$" . markdown-mode) auto-mode-alist))
  )

;; Tern for JS
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; JSX mode
(when (require 'jsx-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
  (require 'flycheck)

  (flycheck-define-checker jsxhint-checker
    "A JSX syntax and style checker based on JSXHint."
    :command ("jsxhint" source)
    :error-patterns
    ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
    :modes (jsx-mode))
  (add-hook 'jsx-mode-hook (lambda ()
                             (flycheck-select-checker 'jsxhint-checker)
                             (flycheck-mode)))
  (setq jsx-indent-level MY-JS-INDENT)

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 4)

  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))
  )

;; ACK -- fallback search
(when (require 'ack-and-a-half nil 'noerror)
  (progn
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
    (global-set-key (kbd "C-x C-a") 'ack))
  )

;; AG -- preferred search
(when (require 'ag nil 'noerror)
  (progn
    (setq ag-reuse-buffers 't)
    (global-set-key (kbd "C-x C-a") 'ag))
  )

;; web-mode
(when (require 'web-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.php" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.json" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.bml" . xml-mode))
  )


;; skewer-mode for browser mind control
(when (require 'skewer-mode nil 'noerror)
  (require 'skewer-repl)
  (require 'skewer-html)
  (require 'skewer-css)
  (defun skewer-start ()
    (interactive)
    (let ((httpd-port 8023))
      (httpd-start)
      (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))
  )

;; clojure's nrepl
(when (require 'nrepl nil 'noerror)
  ;; this is needed to prevent ac-nrepl from breaking
  ;; clojure-mode's starting of nrepl-interaction mode
  ;; on nrepl-jack-in
  (setq nrepl-connected-hook (reverse nrepl-connected-hook))

  ;; disable stack traces outside of repl
  (setq nrepl-popup-stacktraces nil)
  ;; rainbow parens
  (add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)
  ;; hide special buffers
  (setq nrepl-hide-special-buffers t)
  )

;; projectile
(when (require 'projectile nil 'noerror)
  (projectile-global-mode t)
  (setq projectile-completion-system 'ido)
  (global-set-key (kbd "C-c h") 'helm-projectile)
  (global-set-key (kbd "C-c t") 'helm-cmd-t)
  )

;; EIN for Python
(when (require 'ein nil 'noerror)
  (progn
    (setq ein:use-auto-complete t))
  )

;; Find file in project
(when (require 'find-file-in-project nil 'noerror)
  (progn
    (setq ffip-patterns '("*.html" "*.org" "*.md" "*.el" "*.clj" "*.py" "*.rb" "*.js" "*.pl" "*.sh" "*.erl" "*.hs" "*.ml" "*.php" "*.html" "*.phtml"))
    (setq ffip-limit 4096)
    (global-set-key (kbd "C-x f") 'find-file-in-project))
  )

;; IDO
(when (require 'ido nil 'noerror)
  ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
  (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys)
  (setq ido-enable-flex-matching t)
  )

;; hideshow mode
(when (require 'hideshow nil 'noerror)
  (global-set-key (kbd "<backtab>") 'hs-hide-all)
  (global-set-key (kbd "C-<tab>") 'hs-toggle-hiding))

;; ORG-MODE
(when (require 'org nil 'noerror)
  (progn
    (if (not (boundp 'MY-ORG-DIRECTORY)) (setq MY-ORG-DIRECTORY "~/writing"))

    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-c c") 'org-capture)
    (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c b") 'org-iswitchb)
    (setq org-startup-indented 1)
    (setq org-support-shift-select 1)
    (setq org-pretty-entities 1)
    ;; (setq org-todo-keywords
    ;;       '((sequence "TODO" "IN-PROGRESS" "|" "DONE" "DEFERRED")))

    (setq org-directory MY-ORG-DIRECTORY)
    (setq org-default-notes-file (concat org-directory "/notes.org"))
    ;; soft wrap lines
    (defun soft-wrap-lines ()
      "Make lines wrap at window edge and on word boundary, in current buffer."
      (interactive)
      (setq truncate-lines nil)
      (setq word-wrap t))
    (add-hook 'org-mode-hook 'soft-wrap-lines)

    ;; capture templates
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline (concat org-directory "/dailytodo.org") "UNASSIGNED")
             "** TODO %?\n  %i\n")
            ("d" "Draft" entry (file+datetree (concat org-directory "/drafts.org"))
             "* Entered on %U\n  %i\n")))
    ;; agenda files
    ;; (setq org-agenda-files (quote ("~/writing/entropy.org" "~/writing/ideas.org" "~/writing/projects.org" "~/writing/dailytodo.org")))

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
    (setq org-mobile-inbox-for-pull (concat org-directory "/flagged.org"))
    ;; Set to <your Dropbox root directory>/MobileOrg.
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg"))
  )


;; js2-mode
(when (require 'js2-mode nil 'noerror)
  (if (not (boundp 'MY-JS-INDENT)) (setq MY-JS-INDENT 2))

  (setq-default js2-auto-indent-p nil)
  (setq-default js2-global-externs '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "define"))
  (add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))

  ;; from http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
  (defun my-js2-indent-function ()
    (interactive)
    (save-restriction
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (save-excursion (syntax-ppss (point-at-bol))))
             (offset (- (current-column) (current-indentation)))
             (indentation (js--proper-indentation parse-status))
             node)

        (save-excursion
          ;; I like to indent case and labels to half of the tab width
          (back-to-indentation)
          (if (looking-at "case\\s-")
              (setq indentation (+ indentation (/ espresso-indent-level 2))))

          ;; consecutive declarations in a var statement are nice if
          ;; properly aligned, i.e:
          ;;
          ;; var foo = "bar",
          ;;     bar = "foo";
          (setq node (js2-node-at-point))
          (when (and node
                     (= js2-NAME (js2-node-type node))
                     (= js2-VAR (js2-node-type (js2-node-parent node))))
            (setq indentation (+ MY-JS-INDENT indentation))))

        (indent-line-to indentation)
        (when (> offset 0) (forward-char offset)))))

  ;; (add-hook 'javascript-mode-hook
  ;;           (lambda () (flymake-mode t)))

  (setq js2-mode-hook nil)
  (add-hook 'js2-mode-hook
            (lambda ()
              ;; also from http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
              ;;(c-toggle-auto-newline 0)
              ;;(c-toggle-hungry-state 1)
              ;;(set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
              ;;(define-key js2-mode-map [(return)] 'newline-and-indent)
              ;;(define-key js2-mode-map [(backspace)] 'c-electric-backspace)
              ;;(define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
              ;; (if (featurep 'js2-highlight-vars)
              ;;     (js2-highlight-vars-mode))
              ;; TODO -- add environment switch for home/work 2/4 space tabs
              (setq js2-basic-offset MY-JS-INDENT)
              (setq js-indent-level MY-JS-INDENT)
              (setq js2-bounce-indent-p t)
              ;;(tern-mode t)
              (setq c-basic-offset MY-JS-INDENT)

              (setq indent-tabs-mode nil)
              (define-key js2-mode-map (kbd "C-c C-f") nil)
              (define-key js2-mode-map (kbd "<backtab>") 'js2-mode-toggle-element)
              (define-key js2-mode-map (kbd "C-x <tab>") 'js2-mode-toggle-hide-functions)))
  )


;; slime-mode
(when (require 'slime-mode nil 'noerror)
  ;; no need to highlight trailing whitepsace in the repl
  (add-hook 'slime-repl-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  ;; add auto-completion for slime
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  )

;; php-mode
(when (require 'php-mode nil 'noerror)
  (progn
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
                               (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close))))
  )


;; C++ irony mode
(when (require 'irony-mode nil 'noerror)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

;; DEFT for quick notes
(when (require 'deft nil 'noerror)
  (define-minor-mode deft-note-mode "Deft notes" nil " Deft-Notes" nil)
  (setq deft-text-mode 'deft-note-mode)
  (defun kill-all-deft-notes ()
    (interactive)
    (save-excursion
      (let((count 0))
        (dolist(buffer (buffer-list))
          (set-buffer buffer)
          (when (not (eq nil deft-note-mode))
            (setq count (1+ count))
            (kill-buffer buffer)))
        )))
  (defun deft-or-close () (interactive) (if (or (eq major-mode 'deft-mode) (not (eq nil deft-note-mode)))
                                            (progn (kill-all-deft-notes) (kill-buffer "*Deft*"))
                                          (deft)
                                          ))
  (global-set-key [f5] 'deft-or-close)
  )
