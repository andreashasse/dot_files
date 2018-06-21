(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (counsel-projectile counsel swiper elpy json-mode writegood-mode deft which-key swiper-helm cider labburn-theme rainbow-delimiters expand-region git-gutter edts ace-window magit exec-path-from-shell)))
 '(safe-local-variable-values (quote ((allout-layout . t))))
 '(scroll-bar-mode nil)
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
    deft
    writegood-mode
    ace-window
    edts
    git-gutter
    elpy
    expand-region
    which-key
    dashboard
    cider
    json-mode
    ))

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
(add-hook 'after-save-hook 'magit-after-save-refresh-status)

;; org mode
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Dropbox/Andreas/mydocs/todo.org")
         "* TODO %?\n  SCHEDULED: %t\n  %i\n  %a")
        ("r" "Remember" entry (file "~/Dropbox/Andreas/mydocs/remember.org")
         "* %?")
        ("j" "Journal" entry (file+datetree "~/Dropbox/Andreas/mydocs/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("d" "Tech debt" entry (file "~/Dropbox/Andreas/mydocs/tech_debt.org")
         "* %?")
        ("m" "Meeting" entry (file+datetree "~/Dropbox/Andreas/mydocs/journal.org")
         "* %?<project and meeting name>\nNotes\n\nTasks\n\nOpen Questions\n\n")
        ))

(setq org-log-done t)
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))
(setq org-agenda-window-setup 'current-window)

;; ace-windows
(global-set-key (kbd "M-ö") 'ace-window)

(defun prev-window ()
   (interactive)
   (other-window -1))

 (define-key global-map (kbd "C-x p") 'prev-window)

;; git gutter
(global-git-gutter-mode +1)

;;(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-ä") 'counsel-semantic-or-imenu)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(define-key isearch-mode-map (kbd "C-s") 'swiper-from-isearch)
;;(global-set-key (kbd "C-c k") 'counsel-ag)
;;(global-set-key (kbd "C-c p") 'counsel-projectile)
;;(global-set-key (kbd "C-x l") 'counsel-locate)


(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; regex all words
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
;;; C-c C-o (ivy-occur)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

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
;; er/expand-region

;;; Languages
;; Erlang
(require 'edts-start)

;; Python
(elpy-enable)

;; Elisp
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(projectile-mode)
(which-key-mode)
(which-key-setup-minibuffer)

;; Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
((lambda nil (switch-to-buffer "*dashboard*") (goto-char (point-min)) (dashboard-mode)))
(defun show-dashboard (&optional none)
  (interactive "P")
  ((lambda nil (switch-to-buffer "*dashboard*") (goto-char (point-min)) (dashboard-refresh-buffer))))
(global-set-key (kbd "C-å") 'show-dashboard)

;; deft
(setq deft-directory "~/Dropbox/Andreas/mydocs")
(setq deft-use-filename-as-title t)
(setq deft-extensions '("txt" "tex" "org" "clj" "erl"))
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(global-set-key (kbd "M-å") 'deft)

(setq flyspell-issue-welcome-flag nil)
(setq-default ispell-list-command "list")

(defun setup-screen (num)
  (interactive "nHow many windows? ")
  (keyboard-escape-quit)
  (dotimes (i (- num 1))
    (split-window-right))
  (balance-windows))


;; I never code .js files and I'm too lazy to figure out how to properly override
;; json-reformat:indent-width so simply set js-indent-level to 2
;; json-mode uses js-indent-level to set the json indent and this works...
(setq js-indent-level 2)
