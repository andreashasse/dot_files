(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (cider helm-projectile labburn-theme rainbow-delimiters expand-region helm-swoop git-gutter edts ace-window magit exec-path-from-shell)))
 '(safe-local-variable-values (quote ((allout-layout . t))))
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
  '(exec-path-from-shell
    rainbow-delimiters
    labburn-theme
    magit
    ace-window
    edts
    git-gutter
    helm-swoop
    helm-projectile
    expand-region
    cider))

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

;; Global keybindings
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\M-/" 'hippie-expand)

;; Annoying stuff
(setq ring-bell-function 'ignore)
(toggle-scroll-bar -1)
(setq scroll-conservatively 1) ;; Scroll line by line
(setq create-lockfiles nil)

;; y/n before kill emacs
(defun death (&optional none)
  (interactive "P")
  (let ((foo (read-from-minibuffer "DEATH: y/n:")))
    (if (equal foo "y")
        (save-buffers-kill-emacs))))

(global-set-key "\C-x\C-c" 'death)

;; Editing
(setq-default mouse-yank-at-point t)
(put 'overwrite-mode 'disabled t)
(show-paren-mode t)
(column-number-mode t)
(line-number-mode t)

;;; Themes
(require 'labburn-theme)
(set-face-attribute 'whitespace-space nil
                    :background nil
                    :foreground "gray30")
(set-face-attribute 'fringe nil :background "gray30" :foreground nil)

(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "Andale Mono-12")
  (set-face-attribute 'default nil :font "Ubuntu Mono-10"))

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
;; (global-set-key (kbd "C-x g") 'magit-status) use C-c p v

;; org mode
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Dropbox/mydocs/todo.org")
         "* TODO %? %t\n  %i\n  %a")
        ("r" "Remember" entry (file "~/Dropbox/mydocs/remember.org")
         "* %?")
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
(global-set-key (kbd "C-s") 'helm-swoop)
;;(setq helm-swoop-use-fuzzy-match t)
(global-set-key (kbd "M-ä") 'helm-semantic-or-imenu)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
;; (global-set-key (kbd "C-x M-f") 'helm-projectile) use C-c p h

;; Language environment
(set-terminal-coding-system 'iso-8859-1)
(setq default-buffer-file-coding-system 'iso-8859-1)
(prefer-coding-system 'iso-8859-1)
(set-language-environment "Latin-1")
(setq file-buffer-coding 'iso-8859-1)

;; disable backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq backup-inhibited t)

;; Other good commands
;; helm-google-suggest
;; er/expand-region

;;; Languages
;; Erlang
(require 'edts-start)

;; Elisp
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(projectile-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(projectile-mode)
