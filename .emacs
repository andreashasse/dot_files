(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(magit-diff-section-arguments (quote ("--no-ext-diff")))
 '(menu-bar-mode nil)
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (company-lsp lsp-ui lsp-mode erlang rjsx-mode counsel-projectile projectile flx-ido json-mode which-key elpy ace-window magit labburn-theme rainbow-delimiters exec-path-from-shell use-package company)))
 '(safe-local-variable-values (quote ((allout-layout . t))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(whitespace-style
   (quote
    (face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark))))

;;; Packages
(require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/") t)

(setq package-archives
      '(("GNU ELPA" . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("MELPA" . 5)
        ("GNU ELPA" . 0)))

(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(defvar my-packages
  '(company
    use-package
    exec-path-from-shell
    rainbow-delimiters
    labburn-theme
    magit
    ace-window
    elpy
    which-key
    json-mode
    flx-ido
    projectile
    counsel-projectile
    swiper
    rjsx-mode
    ;; erlang
    erlang
    yasnippet
    lsp-mode
    lsp-ui
    company-lsp
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
(setq whitespace-line-column 80)
(global-whitespace-mode t)
(setq-default indent-tabs-mode nil)

;; Set the shell emacs uses.
;(setq explicit-shell-file-name "/bin/bash")

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
                    :foreground "gray40")
(set-face-attribute 'fringe nil :background "gray30" :foreground nil)

(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "Monaco-12")
  (set-face-attribute 'default nil :font "Ubuntu Mono-10"))

(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

(exec-path-from-shell-initialize)

;; magit
;; (global-set-key (kbd "C-x g") 'magit-status) use C-c p v
;;(add-hook 'after-save-hook 'magit-after-save-refresh-status)

(setq magit-push-current-set-remote-if-missing t)


;; ace-windows
(global-set-key (kbd "M-ö") 'ace-window)
(global-set-key (kbd "C-x 0") 'ace-delete-window)

(setq aw-keys '(?j ?k ?l ?ö ?ä ?n ?m))
(setq aw-scope 'frame)

(defun prev-window ()
   (interactive)
   (other-window -1))

(define-key global-map (kbd "C-x p") 'prev-window)

(setq projectile-completion-system 'ivy)

 (defun swiper--from-isearch ()
      "Invoke `swiper' from isearch."
      (interactive)
      (let (($query (if isearch-regexp
                        isearch-string
                      (regexp-quote isearch-string))))
        (isearch-exit)
        (swiper $query)))

;(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-ä") 'counsel-semantic-or-imenu)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(define-key isearch-mode-map (kbd "C-s") 'swiper--from-isearch)
;; C-c g - find file in git repo


(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-initial-inputs-alist nil)

;; regex all words
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
;;; C-c C-o (ivy-occur)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; disable backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq backup-inhibited t)

;; Other good commands

;;; Languages

;; Erlang
;; Install the yasnippet dependency
;(package-install 'erlang)
;(package-install 'yasnippet)

;; ----- lsp-mode -----
;; Install the official lsp-mode package (minimum required version 6.2)
;(package-install 'lsp-mode)
;; Set path to erlang_ls escript (unless it's in your PATH)
; (setq lsp-erlang-server-path "/Users/andreas/build/bin")
;; Enable LSP automatically for Erlang files
(add-hook 'erlang-mode-hook #'lsp)

;; ----- lsp-ui -----
;; It is usually a good idea to install lsp-ui as well
;(package-install 'lsp-ui)
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; The lsp-ui sideline can become a serious distraction, so you
;; may want to disable it
(setq lsp-ui-sideline-enable nil)
;; Ensure docs are visible
(setq lsp-ui-doc-enable t)
;(setq lsp-ui-doc-header t)
;(setq lsp-ui-doc-use-childframe nil)
(setq lsp-ui-doc-delay 0.2)

;; ----- company-lsp -----
;; Enables better integration with company (auto-completion)
(add-hook 'after-init-hook 'global-company-mode)
;(package-install 'company-lsp)
(require 'company-lsp)
(push 'company-lsp company-backends)
(setq lsp-log-io t)

(global-set-key (kbd "C-c C-n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c C-p") 'flymake-goto-prev-error)
; C-c d otp doc
(global-set-key (kbd "C-c D") 'lsp-ui-doc-show)
(global-set-key (kbd "C-c w") 'lsp-find-references)
(global-set-key (kbd "C-c W") 'lsp-ui-peek-find-references)


;; Python
(elpy-enable)
(add-hook 'elpy-mood-hook
          (lambda ()
            (setq-local whitespace-line-column 99)))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; pyvenv-activate <dir of venv>

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))


(projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-enable-caching t)

(which-key-mode)
(which-key-setup-minibuffer)

(defun setup-screen (num)
  (interactive "nHow many windows? ")
  (keyboard-escape-quit)
  (dotimes (i (- num 1))
    (split-window-right))
  (balance-windows))

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-file-extensions-order '(".erl" ".hrl" ".org" ".txt" ".py" ".emacs"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
