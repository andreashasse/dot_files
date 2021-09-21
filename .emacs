;; -*- lisp -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("5e08fb7b2567442909bb538146110264afc0d8351539abd6640d2441ec812250" default))
 '(menu-bar-mode t)
 '(package-selected-packages
   '(php-mode magit-delta org markdown-preview-mode rjsx-mode json-mode py-autopep8 company-jedi elpy org-babel-eval-in-repl ob-sh pug-mode markdown-mode diff-hl deft rainbow-mode rainbow-delimiters yasnippet smex counsel-projectile magit exec-path-from-shell projectile ace-window labburn-theme which-key lsp-ui yasnippet lsp-mode erlang))
 '(safe-local-variable-values '((allout-layout . t)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(whitespace-style
   '(face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))

;; Use packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 27)
  (package-initialize))

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

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

;;(global-diff-hl-mode)

;; Editing
(setq-default mouse-yank-at-point t)
(put 'overwrite-mode 'disabled t)
(show-paren-mode t)
(column-number-mode t)
(line-number-mode t)
(setq-default indent-tabs-mode nil)
(setq whitespace-line-column 100)
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
(load-theme 'labburn t)
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
(setq projectile-sort-order 'recently-active)

(projectile-register-project-type 'kred '(".klarna-system-metadata.json")
                  :compile "make -sj"
                  :test "make myday -sj"
                  :run "bin/kred -shell"
                  :src-dir "src/"
                  :test-dir "test/"
                  :test-suffix "_tests")

(projectile-register-project-type 'pipfile '("Pipfile")
                  :compile "pipenv lint"
                  :test "pipenv run pytest"
                  :run "pipenv shell"
                  :test-prefix "test_")

(global-set-key (kbd "C-ö i") 'counsel-semantic-or-imenu)
(global-set-key (kbd "C-ö t") 'projectile-find-implementation-or-test-other-window)
(global-set-key (kbd "C-ö s") 'swiper-isearch-thing-at-point)
(global-set-key (kbd "C-ö d") 'deft)

(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c j") 'counsel-projectile-git-grep)
(define-key isearch-mode-map (kbd "C-s") 'swiper-from-isearch)
;; C-c g - find file in git repo

;; ORG
(package-require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files (list "~/work/notes/todo_work.org"
                             "~/work/notes/todo_priv.org"))
(setq org-agenda-window-setup 'current-window)
(setq org-log-done t)

;; DEFT
(package-require 'deft)
(setq deft-extensions '("org" "txt" "tex"))
(setq deft-default-extension "org")
(setq deft-directory "~/work/notes")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)
(setq deft-text-mode 'org-mode)
(setq deft-use-filter-string-for-filename t)

(setq deft-file-naming-rules
      '((noslash . "-")
        (nospace . "-")
        (case-fn . downcase)))

;; LSP
;; Include the Language Server Protocol Clients
(package-require 'lsp-mode)
;; completions
(package-require 'company)
(package-require 'lsp-ivy)
;; Include the Yasnippet templating system
(package-require 'yasnippet)
(yas-global-mode t)
;; Enable logging for lsp-mode
(setq lsp-log-io t)
;; Enable code completion
(package-require 'exec-path-from-shell)
(exec-path-from-shell-initialize)


(setq split-width-threshold nil)
(setq split-height-threshold nil)


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

(add-to-list 'load-path "~/.emacs.d/site-packages")

;; Show line and column numbers
(add-hook 'erlang-mode-hook 'linum-mode)
(add-hook 'erlang-mode-hook 'column-number-mode)
(add-hook 'erlang-mode-hook 'rainbow-mode)

(add-hook 'erlang-mode-hook 'lsp)

;; JAVASCRIPT
(package-require 'json-mode)
(package-require 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; PYTHON
(package-require 'elpy)
(elpy-enable)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; Enable autopep8
(package-require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; ELIXIR
;(package-require 'elixir-mode)
;(package-require 'lsp-elixir)
;(add-hook 'elixir-mode-hook 'lsp)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
