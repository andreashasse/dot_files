(custom-set-variables
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

;;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(defvar my-packages
  '(rainbow-delimiters
    exec-path-from-shell
    dash
    magit
    ace-window
    edts
    git-gutter
    helm-swoop
    expand-region))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;; OSX
;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Name and E-mail
(setq user-full-name "Andreas Hasselberg")
(setq user-mail-address "andreas.hasselberg@gmail.com")

;; Editing
(global-whitespace-mode t)
(setq-default indent-tabs-mode nil)

;; Set the shell emacs uses.
(setq explicit-shell-file-name "/bin/bash")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keybindings
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\M-/" 'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annoying stuff
;; (setq visible-bell t)
(setq ring-bell-function 'ignore)
(scroll-bar-mode nil)
;; Scroll line by line
(setq scroll-conservatively 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
(setq-default mouse-yank-at-point t)
(put 'overwrite-mode 'disabled t)
(show-paren-mode t)
(column-number-mode t)
(line-number-mode t)

(load "~/erlang-conf.el")

(defun death (&optional none)
  (interactive "P")
  (let ((foo (read-from-minibuffer "DEATH: y/n:")))
    (if (equal foo "y")
        (save-buffers-kill-emacs))))

(global-set-key "\C-x\C-c" 'death)

(custom-set-faces
 '(default ((t (:family "Ubuntu Mono"
                        :foundry "unknown"
                        :slant normal
                        :weight normal
                        :height 90
                        :width normal)))))

(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "Andale Mono-12"))

(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;; all buffers, try to reuse windows across all frames
(add-to-list 'display-buffer-alist
           '(".*". (display-buffer-reuse-window .
                                  ((reusable-frames . t)))))

;; except for compilation buffers where you want new and dedicated frames when necessary
(add-to-list 'display-buffer-alist
         '("^\\*Compile-Log\\*". ((display-buffer-reuse-window
                                   display-buffer-pop-up-frame) .
                                  ((reusable-frames . t)
                                  (inhibit-same-window . t)))))

(exec-path-from-shell-initialize)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;
; org mode
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Dropbox/mydocs/todo.org")
         "* TODO %? %t\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Dropbox/mydocs/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; ace-windows
(global-set-key (kbd "M-ö") 'ace-window)

;; git gutter
(global-git-gutter-mode +1)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C--") 'helm-swoop)
(setq helm-swoop-use-fuzzy-match t)

;; disable backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq backup-inhibited t)

;; More good stuff
;; helm-semantic-or-imenu
;; 
;; helm-google-suggest
;; er/expand-region
