;;; .emacs --- Personal Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Built for Emacs 30+. Uses the built-in tools where they are good enough:
;; use-package, eglot (LSP), tree-sitter modes, project.el and which-key.
;; The minibuffer stack is Vertico + Orderless + Marginalia + Consult + Embark.
;; Elixir uses Dexter as language server.
;; Keybindings are the standard Emacs ones. Packages take over standard keys
;; (C-x b, M-y, M-g g ...) instead of adding new ones.

;;; Code:

;;;; Packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; Keep Custom from writing into this file.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;;;; Basics

(setq user-full-name "Andreas Hasselberg"
      user-mail-address "andreas.hasselberg@gmail.com")

;; Command is Meta, Option types special characters.
(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      use-short-answers t
      scroll-conservatively 101
      split-height-threshold nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)

(setq-default indent-tabs-mode nil
              fill-column 100)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; No backup, lock or auto-save files next to the source.
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Ask before quitting.
(setq confirm-kill-emacs #'y-or-n-p)

;; Load PATH from the login shell, so that mise tools (elixir, dexter, elp)
;; are found when Emacs starts from the Dock.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;;;; Look

;; Gruvbox Dark, like Zed. The Doom version also colors the newer
;; tree-sitter faces (function calls, properties, operators).
(use-package doom-themes
  :config (load-theme 'doom-gruvbox t))

;; Color as much as tree-sitter can, like Zed does: also function calls,
;; variables, operators and brackets.
(setq treesit-font-lock-level 4)

;; Mark the indentation levels with thin lines, like Zed.
(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :config (setq indent-bars-treesit-support t))

(set-face-attribute 'default nil
                    :family (if (find-font (font-spec :family "FiraCode Nerd Font Mono"))
                                "FiraCode Nerd Font Mono"
                              "Menlo")
                    :height 130)

(use-package which-key
  :ensure nil
  :config (which-key-mode 1))

;;;; Dired

;; macOS ls does not support --dired. Use GNU ls (gls, from coreutils)
;; when it is installed, and otherwise turn the option off.
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (if (executable-find "gls")
      (setq insert-directory-program "gls"
            dired-listing-switches "-alh --group-directories-first")
    (setq dired-use-ls-dired nil
          dired-listing-switches "-alh"))
  ;; ( toggles the details (permissions, owner, size, date).
  (setq dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t))

;;;; Windows

(use-package ace-window
  :bind (("M-ö" . ace-window)
         ("C-x o" . ace-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-scope 'frame))

;;;; Minibuffer completion

;; Vertical candidate list in the minibuffer.
(use-package vertico
  :init (vertico-mode 1)
  :config (setq vertico-cycle t))

;; Match candidates by space separated parts in any order.
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Extra information next to each candidate (doc strings, file sizes ...).
(use-package marginalia
  :init (marginalia-mode 1))

;; Search and navigation commands with live preview.
(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x p b" . consult-project-buffer)
         ("M-y"     . consult-yank-pop)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("M-t"     . consult-imenu)     ; replaces transpose-words
         ("M-g I"   . consult-imenu-multi)
         ("M-g f"   . consult-flymake)
         ("M-g o"   . consult-outline)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s r"   . consult-ripgrep)
         ("M-s g"   . consult-git-grep)
         ("M-s f"   . consult-fd)
         :map isearch-mode-map
         ("M-s l"   . consult-line))
  :config
  ;; Use Consult to pick between several xref results (M-. and M-?).
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Actions on the thing at point or the current candidate. In the minibuffer,
;; C-. E exports the candidates to a buffer (for example a grep buffer).
(use-package embark
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings))
  :config (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Edit grep results in place: export with C-. E, then C-c C-p, edit, C-c C-c.
(use-package wgrep)

;;;; In-buffer completion

(use-package corfu
  :init (global-corfu-mode 1)
  :config (setq corfu-auto t
                corfu-auto-delay 0.2
                corfu-cycle t))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :config (corfu-popupinfo-mode 1))

;; TAB indents first, then completes.
(setq tab-always-indent 'complete)

;;;; Selection

;; Grow or shrink the selection along the syntax tree: M-o expands one
;; level, M-O shrinks one level. In tree-sitter modes only syntax nodes
;; count, like Zed. Other modes also use words, lists and paragraphs.
(defun my/expreg-treesit-only ()
  "Make expreg step through tree-sitter nodes only, when the mode has a parser."
  (when (and (fboundp 'treesit-parser-list) (treesit-parser-list))
    (setq-local expreg-functions '(expreg--treesit))))

(use-package expreg
  :bind (("M-o" . expreg-expand)
         ("M-O" . expreg-contract))
  :hook (prog-mode . my/expreg-treesit-only))

;;;; Git

(use-package magit
  :bind ("C-x g" . magit-status))

;; Git changes in the left fringe, in two kinds:
;; - Full marks: uncommitted changes (staged or not), updated while you type.
;; - Flat, darker marks: lines that commits on this branch changed, compared
;;   with the point where the branch left the default branch.
;; C-x v [ and C-x v ] jump between changes, C-x v * shows the change at point.
(defun my/git-default-branch ()
  "Return the default branch of the remote, such as \"origin/master\"."
  (or (ignore-errors
        (car (process-lines "git" "symbolic-ref" "--short" "refs/remotes/origin/HEAD")))
      (seq-find (lambda (ref)
                  (zerop (call-process "git" nil nil nil
                                       "rev-parse" "--verify" "--quiet" ref)))
                '("origin/main" "origin/master" "main" "master"))))

(defun my/diff-hl-set-branch-reference ()
  "Compare this buffer with the commit where the branch left the default branch."
  (when-let ((root (and buffer-file-name
                         (locate-dominating-file buffer-file-name ".git"))))
    (let* ((default-directory root)
           (base (when-let ((branch (my/git-default-branch)))
                   (ignore-errors
                     (car (process-lines "git" "merge-base" "HEAD" branch))))))
      (unless (equal base diff-hl-reference-revision)
        (setq-local diff-hl-reference-revision base)
        (diff-hl-update)))))

(defun my/diff-hl-refresh-branch-references ()
  "Recompute the branch reference in all buffers, for example after a checkout."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p diff-hl-mode)
        (my/diff-hl-set-branch-reference)))))

(use-package diff-hl
  :init (global-diff-hl-mode 1)
  :hook ((diff-hl-mode . my/diff-hl-set-branch-reference)
         (magit-post-refresh . my/diff-hl-refresh-branch-references)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq diff-hl-update-async t)
  (diff-hl-flydiff-mode 1)
  ;; Darker Gruvbox colors for the branch marks, so they differ from the
  ;; uncommitted ones.
  (set-face-attribute 'diff-hl-reference-insert nil :background "#79740e" :foreground "#79740e")
  (set-face-attribute 'diff-hl-reference-change nil :background "#076678" :foreground "#076678")
  (set-face-attribute 'diff-hl-reference-delete nil :background "#9d0006" :foreground "#9d0006"))

;;;; GitHub

;; Ghub (used by Forge and pr-review) looks for tokens in ~/.authinfo.
;; When none is there, ask the gh CLI, which keeps its token in the keychain.
;; The GitHub username comes from `github.user' in .gitconfig.
(defvar my/gh-token nil)
(with-eval-after-load 'ghub
  (define-advice ghub--token (:around (fn host username package &optional nocreate forge) gh-cli)
    (or (funcall fn host username package t forge)
        (and (memq forge '(nil github))
             (or my/gh-token
                 (let ((token (string-trim
                               (shell-command-to-string "gh auth token 2>/dev/null"))))
                   (unless (string-empty-p token)
                     (setq my/gh-token token)))))
        (funcall fn host username package nocreate forge))))

;; Forge: list, read and check out issues and PRs from Magit.
;; In the Magit status buffer, N opens the Forge menu.
(use-package forge
  :after magit)

;; Review PRs: check out the PR (gh pr checkout N), then C-c r in the
;; project and paste the PR URL. In the review buffer, C-c C-c comments on
;; the line at point (mark a region for several lines), C-c C-g opens the
;; local file at that line, and C-c C-s submits the review.
(use-package pr-review
  :bind (("C-c r" . pr-review)
         :map pr-review-mode-map
         ("C-c C-g" . my/pr-review-visit-local-file)))

(defun my/pr-review-visit-local-file ()
  "Open the local file at the diff line at point in the other window.
The file is looked up in the Git checkout that the review was opened
from, so the language server and project search work there."
  (interactive)
  (pcase (pr-review--get-diff-line-info (point))
    (`(,side ,file . ,line)
     (let ((root (or (locate-dominating-file default-directory ".git")
                     (read-directory-name "Local checkout of the PR: "))))
       (find-file-other-window (expand-file-name file root))
       (goto-char (point-min))
       (forward-line (1- line))
       (when (equal side "LEFT")
         (message "This line was removed in the PR. Showing the same line number in the local file."))))
    (_ (user-error "Move point to a line in the diff"))))

;;;; Tree-sitter

;; Grammars for the built-in *-ts-mode major modes. Missing grammars are
;; built on first start. This needs a C compiler (Xcode Command Line Tools).
(setq treesit-language-source-alist
      '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (heex   "https://github.com/phoenixframework/tree-sitter-heex")))

(dolist (lang (mapcar #'car treesit-language-source-alist))
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

;;;; LSP (eglot)

;; Eglot is the built-in LSP client. With it, the standard keys work:
;; M-. definition, M-? references, M-, back, C-h . documentation.
(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format-buffer)
              ("C-c l i" . eglot-find-implementation))
  :config
  (setq eglot-autoshutdown t
        eglot-events-buffer-config '(:size 0 :format full))
  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode heex-ts-mode) "dexter" "lsp"))
  (add-to-list 'eglot-server-programs
               '(erlang-mode "elp" "server")))

;; Search all symbols in the project through the language server.
(use-package consult-eglot
  :after eglot
  :config (keymap-set eglot-mode-map "C-c l s" #'consult-eglot-symbols))

;; Format with the language server on save, when the server can do it.
(defun my/eglot-format-on-save ()
  "Format the buffer with eglot before save in this buffer."
  (add-hook 'before-save-hook
            (lambda ()
              (when (eglot-server-capable :documentFormattingProvider)
                (eglot-format-buffer)))
            nil t))

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;;;; Elixir

(use-package elixir-ts-mode
  :ensure nil
  :mode (("\\.exs?\\'" . elixir-ts-mode)
         ("mix\\.lock\\'" . elixir-ts-mode))
  :hook ((elixir-ts-mode . eglot-ensure)
         (elixir-ts-mode . my/eglot-format-on-save)))

(use-package heex-ts-mode
  :ensure nil
  :mode "\\.heex\\'"
  :hook ((heex-ts-mode . eglot-ensure)
         (heex-ts-mode . my/eglot-format-on-save)))

;; Run ExUnit tests: C-c , a (all), C-c , v (this file), C-c , s (test at point),
;; C-c , r (run again).
(use-package exunit
  :hook (elixir-ts-mode . exunit-mode))

;;;; Erlang

(use-package erlang
  :hook (erlang-mode . eglot-ensure))

;;;; Other file types

(use-package markdown-mode)
(use-package yaml-mode)

;;; .emacs ends here
