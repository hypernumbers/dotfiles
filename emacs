(add-to-list 'load-path "~/.emacs_lib") 

(require 'http-twiddle)

;; Save sessions and stuff 
(desktop-load-default)
(desktop-save-mode 1)

;; Enable modes
(ido-mode 1)
(cua-mode 1)
(line-number-mode 1)
(column-number-mode 1)

;; Disable some ui
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Set the font
(set-face-attribute 'default nil :height 90)
(set-face-background 'mode-line "black")
(set-face-foreground 'mode-line "white")

;; Some custom keybindings
(global-set-key "\C-f" 'rgrep)
(global-set-key "\C-l" 'goto-line)
(global-set-key "\C-a" 'align-regexp)
(global-set-key "\M-m" 'comment-region)
(global-set-key "\M-n" 'uncomment-region)
(global-set-key "\C-h" 'other-window)
(global-set-key "\M-k" 'kill-this-buffer)
;(global-set-key [f12] 'compile)

;; Stop making ~ files, get rid of startup message
(setq inhibit-startup-message 0)
(setq make-backup-files         nil) 
(setq auto-save-list-file-name  nil) 

;; Set default size
(setq default-frame-alist 
      (append (list '(width  . 80) '(height . 51)) default-frame-alist)) 

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
(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

(add-to-list 'auto-mode-alist '("\\.tpl$" . html-mode))

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

(add-to-list 'load-path "/usr/local/share/wrangler/elisp")
(require 'wrangler)

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
