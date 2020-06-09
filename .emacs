;; -*- lisp -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(menu-bar-mode t)
 '(package-selected-packages
   (quote
    (deft rainbow-mode rainbow-delimiters yasnippet smex counsel-projectile magit exec-path-from-shell projectile ace-window labburn-theme which-key lsp-ui company-lsp yasnippet lsp-mode erlang)))
 '(safe-local-variable-values (quote ((allout-layout . t))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(whitespace-style
   (quote
    (face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark))))

;; Use packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.milkbox.net/packages/") t)
(package-initialize)
(package-refresh-contents)
;; Install a package only if it's not already installed
(defun package-require (pkg &optional require-name)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg))
  (if require-name
      (require require-name)
    (require pkg)))

(setq pop-up-windows nil)

;;; OSX
;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Name and E-mail
(setq user-full-name "Andreas Hasselberg")
(setq user-mail-address "andreas.hasselberg@gmail.com")

(setq ring-bell-function 'ignore)

;; y/n before kill emacs
(defun death (&optional none)
  (interactive "P")
  (let ((foo (read-from-minibuffer "DEATH: y/n:")))
    (if (equal foo "y")
        (save-buffers-kill-emacs))))

(global-set-key "\C-x\C-c" 'death)

;; Magit
(package-require 'magit)

;; Editing
(setq-default mouse-yank-at-point t)
(put 'overwrite-mode 'disabled t)
(show-paren-mode t)
(column-number-mode t)
(line-number-mode t)
(setq-default indent-tabs-mode nil)
(setq whitespace-line-column 80)
(global-whitespace-mode t)
(setq scroll-conservatively 1)


;; disable backups
(setq create-lockfiles nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq backup-inhibited t)

;;; Themes
(package-require 'labburn-theme)
(set-face-attribute 'whitespace-space nil
                    :background nil
                    :foreground "gray40")
(set-face-attribute 'fringe nil :background "gray30" :foreground nil)


(package-require 'rainbow-mode)

;; Ace window (and other window stuff
(package-require 'ace-window)
(global-set-key (kbd "M-ö") 'ace-window)
(global-set-key (kbd "C-x 0") 'ace-delete-window)

(setq aw-keys '(?j ?k ?l ?ö))
(setq aw-scope 'frame)

(defun prev-window ()
   (interactive)
   (other-window -1))

(define-key global-map (kbd "C-x p") 'prev-window)

;; WHICH KEY
(package-require 'which-key)
(which-key-mode)
(which-key-setup-minibuffer)

;; PROJECTILE
(package-require 'projectile)
(package-require 'counsel-projectile)
(projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)

(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-ä") 'counsel-semantic-or-imenu)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(define-key isearch-mode-map (kbd "C-s") 'swiper-from-isearch)
;; C-c g - find file in git repo

;; DEFT
(package-require 'deft)
(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/work/notes")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)
(global-set-key (kbd "C-å") 'deft)


;; LSP
;; Include the Language Server Protocol Clients
(package-require 'lsp-mode)
;; Include the Yasnippet templating system
(package-require 'yasnippet)
(yas-global-mode t)
;; Enable logging for lsp-mode
(setq lsp-log-io t)
;; Enable code completion
(package-require 'company-lsp)
(push 'company-lsp company-backends)
;; Enable diagnostics
(package-require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
;; LSP-UI
(package-require 'lsp-ui)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-position 'bottom)

(global-set-key (kbd "C-c C-n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c C-p") 'flymake-goto-prev-error)

;;(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
;;(global-set-key (kbd "C-c C-p") 'flycheck-prev-error)

; C-c d otp doc
(global-set-key (kbd "C-c D") 'lsp-ui-doc-show)
(global-set-key (kbd "C-c w") 'lsp-find-references)
(global-set-key (kbd "C-c W") 'lsp-ui-peek-find-references)

;; ERLANG
;; Install Erlang mode
(package-require 'erlang)

;; org mode erlang snippets
;; https://github.com/xfwduke/ob-erlang
(add-to-list 'load-path "~/.emacs.d/site-packages")

(require 'ob-erlang)
(org-babel-do-load-languages
    'org-babel-load-languages
    '((erlang . t)))

;; Show line and column numbers
(add-hook 'erlang-mode-hook 'linum-mode)
(add-hook 'erlang-mode-hook 'column-number-mode)
(add-hook 'erlang-mode-hook 'rainbow-delimiters-mode)

(add-hook 'erlang-mode-hook #'lsp)
;; Override the default erlang-compile-tag to use completion-at-point
(eval-after-load 'erlang
  '(define-key erlang-mode-map (kbd "C-M-i") #'company-lsp))



;; JAVASCRIPT
(package-require 'json-mode)
(package-require 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; ELIXIR
(package-require 'elixir-mode)
(package-require 'lsp-elixir)
(add-hook 'elixir-mode-hook 'lsp)



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
