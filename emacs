;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-

(add-to-list 'load-path "~/.emacs_lib") 
(add-to-list 'load-path "~/.emacs_lib/js2mode") 

(require 'http-twiddle)

;; Save sessions and stuff 
(desktop-load-default)
(desktop-save-mode 1)

;; Enable modes
(ido-mode 1)
(cua-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(setq kill-whole-line t)
;; Disable some ui
;;(menu-bar-mode 0)
;;(scroll-bar-mode 0)
;;(tool-bar-mode 0)

;; Set the font
;;(set-face-attribute 'default nil :height 90)
(set-face-background 'mode-line "black")
(set-face-foreground 'mode-line "white")

;; Some custom keybindings
(global-set-key "\C-f" 'rgrep)
(global-set-key "\C-l" 'goto-line)
(global-set-key "\M-a" 'align-regexp)
(global-set-key "\M-m" 'comment-region)
(global-set-key "\M-n" 'uncomment-region)
(global-set-key "\C-h" 'other-window)
(global-set-key "\M-k" 'kill-this-buffer)
;(global-set-key [f12] 'compile)

;; Stop making ~ files, get rid of startup message
(setq inhibit-startup-message 0)
(setq make-backup-files         nil) 
(setq auto-save-list-file-name  nil) 

;; Use clipboard
(setq x-select-enable-clipboard t) 
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Use spaces to indent, 4 by default.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Fancy auto complete box
(require 'auto-complete)
(global-auto-complete-mode 1)
(setq ac-dwim t)
(setq ac-auto-start t)
(setq-default ac-sources '(ac-source-abbrev ac-source-words-in-buffer))
(set-face-background 'ac-candidate-face "gray12")
(set-face-foreground 'ac-candidate-face "gray40")
(set-face-background 'ac-selection-face "gray12")
(set-face-foreground 'ac-selection-face "gray90")
(set-face-background 'ac-completion-face "LightSkyBlue4")
(set-face-foreground 'ac-completion-face "gray12")
(auto-complete-mode 1)

;; Make pretty colours
(add-to-list 'load-path "~/.emacs_lib/color-theme-6.6.0") 
(require 'color-theme)
(color-theme-initialize)
(color-theme-midnight)

;; Espresso-mode for javascript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'espresso-mode "espresso")

;(autoload 'espresso-mode "espresso" "Start espresso-mode" t)

;; (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;; (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl$" . html-mode))

;; CSS color values colored by themselves
;; http://xahlee.org/emacs/emacs_html.html
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)

;; Erlware-mode for erlang
(setq load-path (cons "~/.emacs_lib/erlware-mode" load-path))
(require 'erlang-start)

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yrl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.xrl?$" . erlang-mode))

(add-hook 'erlang-mode-hook
          '(lambda ()
             (flymake-mode)
             (local-set-key (kbd "M-'") 'erlang-flymake-next-error)
             ))

;; Distel also for erlang
(add-to-list 'load-path "~/.emacs_lib/distel/elisp")
(require 'distel)
(distel-setup)
(setq derl-cookie "completelysecure")
(setq erl-nodename-cache (make-symbol "hndev@localhost.com"))

;;(add-to-list 'load-path "/usr/local/share/wrangler/elisp")
;;(require 'wrangler)

;; kill compilation buffer if no errors
;; (setq compilation-finish-function
;;       (lambda (buf str)        
;;         (if (string-match "Termination Status: ok" (buffer-string))
;;             (kill-buffer-and-window)
;;           ())))

(defun show-onelevel ()
 "show entry and children in outline mode"
 (interactive)
 (show-entry)
 (show-children))

(defun my-outline-bindings ()
 "sets shortcut bindings for outline minor mode"
 (interactive)
 (local-set-key [C-up] 'outline-previous-visible-heading)
 (local-set-key [C-down] 'outline-next-visible-heading)
 (local-set-key [C-left] 'hide-subtree)
 (local-set-key [C-right] 'show-onelevel))

(add-hook
 'outline-minor-mode-hook
 'my-outline-bindings)

(add-hook
 'erlang-mode-hook
 '(lambda ()
   (outline-minor-mode)
   (setq outline-regexp
         (concat "^-?" erlang-atom-regexp "\\s *("))))


;; Testing Junk
(defconst erlang-keywords 
  (sort 
   (list "bumkins" ) #'(lambda (a b) (> (length a) (length b)))))

(defvar ac-source-erlang
  '((candidates
     . (lambda ()
         (all-completions ac-target erlang-keywords))))
  "Source for erlang keywords.")

(add-hook 'erlang-mode-hook
          (lambda ()
            (make-local-variable 'ac-sources)
            (setq ac-sources '(ac-source-erlang 
                               ac-source-abbrev 
                               ac-source-words-in-buffer))))

(defun build-hypernumbers ()
  (interactive)
  (cd "/home/dale/lib/hypernumbers/")
  (compile "./hn quick")
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(global-set-key [f12] 'build-hypernumbers)


(defun build-erlangotp ()
  (interactive)
  (cd "/home/dale/lib/erlangotp.com/")
  (compile "./ctrl build")
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(global-set-key [f11] 'build-erlangotp)


(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
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
          (setq indentation (+ 4 indentation))))
      
      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))


(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 8)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)] 
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
    (js2-highlight-vars-mode))
  (message "My JS2 hook"))

;; (add-hook 'js2-mode-hook 
;;           'my-js2-mode-hook
;;           )

(require 'flymake-jslint)
(add-hook 'js2-mode-hook
          '(lambda () (flymake-mode t)))
