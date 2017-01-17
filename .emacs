(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(safe-local-variable-values
   (quote
    ((st-rulers .
                [70])
     (indent-tabs-mode . 1)
     (allout-mode . t)
     (allout-layout . t))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

;; fix the PATH variable (osx)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/home/andreas/build/otp-r17/release/bin"))
(setq exec-path (append exec-path '("/usr/local/bin"
                                    "/home/andreas/build/otp-r17/release/bin")))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(defvar my-packages
  '(better-defaults
    rainbow-delimiters
    exec-path-from-shell
    dash
    magit
    ace-window
    edts
    git-gutter
    helm-swoop))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name and E-mail
(setq user-full-name "Andreas Hasselberg")
(setq user-mail-address "andreas.hasselberg@gmail.com")

(global-whitespace-mode t)
(setq-default indent-tabs-mode nil)

;; Set the shell emacs uses.
(setq explicit-shell-file-name "/bin/bash")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keybindings
(global-set-key "\C-c\C-c" 'comment-region)
;;(global-set-key "\M-t" 'erl-complete)
;;(global-set-key "\M-/" 'dabbrev-completion)
(global-set-key "\C-t" 'other-window)
(global-set-key "\M-g" 'goto-line)  ; Goto line number
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key "\M-/" 'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annoying stuff
;; (setq visible-bell t)
(setq ring-bell-function 'ignore)
(scroll-bar-mode nil)
;; Scroll line by line
(setq scroll-conservatively 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
(setq completion-ignore-case t
      pcomplete-ignore-case t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
(setq-default mouse-yank-at-point t)
(put 'overwrite-mode 'disabled t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language environment
(set-terminal-coding-system 'iso-8859-1)
(setq default-buffer-file-coding-system 'iso-8859-1)
(prefer-coding-system 'iso-8859-1)
(set-language-environment "Latin-1")
(setq file-buffer-coding 'iso-8859-1)
(show-paren-mode t)
;; (let ((mode (current-input-mode)))
;;   (setcar (cdr (cdr mode)) 8)
;;   (apply 'set-input-mode mode))
; (iso-accents-mode) C-x 8 /a -> å
(let ((mode (current-input-mode)))
  (setcar (cdr (cdr mode)) 8)
  (apply 'set-input-mode mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Modes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global modes
(column-number-mode t)
(line-number-mode t)
(transient-mark-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock mode
;;(require 'font-lock)
;;(global-font-lock-mode t)
;;(setq font-lock-maximum-decoration t)

(load "~/erlang-conf.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun death (&optional none)
  (interactive "P")
  (let ((foo (read-from-minibuffer "DEATH: y/n:")))
    (if (equal foo "y")
        (save-buffers-kill-emacs))))

(global-set-key "\C-x\C-c" 'death)
;;(global-set-key "\C-x\C-r" 'register-to-point)
;;(global-set-key "\C-x\C-p" 'point-to-register)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal)))))


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

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-default nil)
(setq make-backup-files nil)

(exec-path-from-shell-initialize)
(require 'better-defaults)

;; org mode
(setq org-default-notes-file "~/work/mydocs/notes.org")
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)

;; ace-windows
(global-set-key (kbd "M-ö") 'ace-window)

;; git gutter
(global-git-gutter-mode +1)
;; (global-set-key (kbd "C-x C-g") 'git-gutter)
;; (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;; (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
;; (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
;; (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)


(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C--") 'helm-swoop)
