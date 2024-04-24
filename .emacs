;; -*- lisp -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("5e08fb7b2567442909bb538146110264afc0d8351539abd6640d2441ec812250" default))
 '(package-selected-packages '(rainbow-mode magit labburn-theme))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(whitespace-style
   '(face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))

;; Package management

;; Add MELPA to package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;(package-refresh-contents)

;; Set up use-package to automate package installation
(unless (package-installed-p 'use-package)
  ;; first of all ensure that use-package itself is installed
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package)
                   (require 'use-package-ensure)
                   (setq use-package-always-ensure t))

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
(use-package magit)

;;(global-diff-hl-mode)

;; Editing
(setq-default mouse-yank-at-point t)
(put 'overwrite-mode 'disabled t)
(show-paren-mode t)
(column-number-mode t)
(line-number-mode t)
(setq-default indent-tabs-mode nil)
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
(use-package labburn-theme)
(load-theme 'labburn t)
(set-face-attribute 'whitespace-space nil
                    :background nil
                    :foreground "gray40")
(set-face-attribute 'fringe nil :background "gray30" :foreground nil)
