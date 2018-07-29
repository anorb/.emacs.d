;;; init.el --- My Emacs config                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Austin Norberg

;; Author: Austin Norberg <austin@norberg.tech>

;;; Commentary:

;;

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

;; Load separately stored custom file
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq user-full-name "Austin Norberg"
      user-mail-address "austin@norberg.tech")

;; Code formatting
(setq tab-width 4)
(defvar c-default-style "k&r")
(defvar c-basic-offset 2)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)

;; GPG stuff
(setq epg-gpg-program "gpg2")
(setq epg-gpg-home-directory "~/.gnupg")

;; Tramp stuff
(defvar tramp-default-method "ssh")

;; UI stuff
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(column-number-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p) ; Make yes/no prompts shorter
(show-paren-mode 1)               ; Highlight parenthesis & other characters
(global-hl-line-mode 1)           ; Highlight current line

;; Typing with region selected will delete region
(delete-selection-mode)

;; Going to a new line at the end of a buffer creates new line
(setq next-line-add-newlines t)

;; Save buffers & window config on exit
(desktop-save-mode 1)

;; Kills dired buffers on emacs close
(add-hook 'kill-emacs-hook #'kill-dired-buffers)

;; ensure all package with use-package
(defvar use-package-always-ensure t)

;; Backups and autosaves to tmp
;; (setq backup-directory-alist
;; 	  `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;; 	  `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old

;; Ibuffer changes
(defvar ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "default")))

(defvar ibuffer-saved-filter-groups
	'(("default"
		 ("dired" (mode . dired-mode))
		 ("org" (name . "^.*org$"))
		 ;; ("web" (or (mode . web-mode) (mode . js2-mode)))
		 ("shell" (or (mode . eshell-mode)
                              (mode . shell-mode)))
		 ("programming" (or
                                 (mode . sh-mode)
                                 (mode . lisp-mode)
                                 (mode . emacs-lisp-mode)
                                 (mode . go-mode)
                                 (mode . js2-mode)))
		 ("emacs" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")
                           (name . "^\\*Help\\*$"))))))

;;; Load misc-defuns for keybinds
(load "~/.emacs.d/lisp/misc-defuns.el")

;;; Custom keybinds
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-v") 'scroll-up-half)
(global-set-key (kbd "C-v") 'scroll-down-half)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-w") 'xah-cut-line-or-region)
(global-set-key (kbd "M-w") 'xah-copy-line-or-region)

(global-set-key (kbd "H-e") 'eshell)

;; Built in packages
(use-package autorevert
  :delight auto-revert-mode
  :init
  (global-auto-revert-mode t))

(use-package subword
  :delight
  :init
  (global-subword-mode 1))

;;; MELPA packages
(use-package god-mode
  :bind ("<escape>" . god-mode-all)
  :init
  (defun my-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'hbar
                        'box)))
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor))

(use-package elfeed
  :bind (("C-x w" . elfeed)
  :map elfeed-search-mode-map
  ("l" . elfeed-toggle-star))
  :init
  (setq elfeed-db-directory "~/Sync/elfeed/elfeeddb")
  (setq-default elfeed-search-filter "@2-week-ago +unread -star")
  :config
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star)))

(use-package elfeed-org
  :init
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(use-package undo-tree
  :delight
  :bind
  ("<f9>" . 'undo)
  ("<f10>" . 'redo)
  :init
  (global-undo-tree-mode))

(use-package git-gutter
  :init
  (global-git-gutter-mode 't)
  :delight)

(use-package projectile
  :init
  (projectile-mode)                     ; Enable projectile mode in all buffers
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line
        '(:eval (if (projectile-project-p)
                    (format " [%s]"
                            (projectile-project-name))
                  ""))))

(use-package eyebrowse
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-z"))
  :config
  (progn
    (define-key eyebrowse-mode-map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
    (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
    (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
    (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
    (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
    (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
    (eyebrowse-mode t)
    (setq eyebrowse-new-workspace t)))

(use-package org-mode
  :bind
  (("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  :map org-mode-map
  ("C-c u u" . org-password-manager-get-username)
  ("C-c u p" . org-password-manager-get-password)
  ("C-c u g" . org-password-manager-generate-password))
  :init
  (defvar org-capture-bookmark nil)
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords '((type "TODO" "|" "DONE")))
  (setq org-hide-leading-stars t)
  (setq org-ellipsis "⤵")

  (setq org-directory "~/Sync/org/")
  (load-library "find-lisp")
  (setq org-agenda-files (find-lisp-find-files org-directory "\.org$"))

  (defvar org-capture-templates
        `(("t" "tech task" entry (file+headline ,(concat org-directory "personal.org") "Tech tasks")
           "* TODO %?\n")
          ("n" "non-tech task" entry (file+headline ,(concat org-directory "personal.org") "Non-tech tasks")
           "* TODO %?\n")
          ("r" "research" entry (file+headline ,(concat org-directory "personal.org") "Research notes")
           "* TODO %?\n")
          ("s" "shopping list" entry (file+headline ,(concat org-directory "personal.org") "Shopping list")
           "* TODO %?\n")
          ("f" "finance task" entry (file+headline ,(concat org-directory "finance.org") "Tasks")
           "* TODO %?\n")
          ("i" "fitness task" entry (file+headline ,(concat org-directory "fitness.org") "Tasks")
           "* TODO %?\n")
          ("e" "emacs" entry (file+headline ,(concat org-directory "emacs.org") "Tasks")
           "* TODO %?\n")))
  (setq org-default-notes-file (concat org-directory "personal.org"))
  (setq org-archive-location (concat org-directory "archive/%s_archive::")))

(use-package js2-mode
  :config
  ;; Turn off js2 mode errors & warnings
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-basic-offset 2)
  (js2-imenu-extras-mode))

(use-package rjsx-mode
  :mode "\\.js\\'")

(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (defvar company-tooltip-align-annotations t)   ; aligns annotation to the right hand side
  (add-hook 'rjsx-mode-hook #'setup-tide-mode))

(use-package web-mode
  :mode ("\\.hbs\\'" "\\.htm\\'" "\\.html\\'")
  :hook (web-mode . indent-guide-mode)
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-script-padding 2
        web-mode-style-padding 2
        web-mode-engines-alist '(("ctemplate"    . "\\.hbs\\'"))))

;; Requires eslint (npm i -g eslint)
(use-package tide
  :delight)

(use-package eldoc
  :delight)

(use-package indent-guide
  :delight)

;; Requires:
;; go get golang.org/x/tools/cmd/goimports
;; go get https://github.com/nsf/gocode
(use-package go-mode
  :bind (:map go-mode-map ("M-." . 'godef-jump))
  :init
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

;; Integration of the 'gorename' tool into Emacs.
;; go get golang.org/x/tools/cmd/gorename
;; go build golang.org/x/tools/cmd/gorename
(use-package go-rename) ; requires go-rename binary

(use-package go-playground
  :init
  (setq go-playground-basedir "~/Projects/go/src/playground"))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package dired-sidebar
  :bind ("<f8>" . dired-sidebar-toggle-sidebar))


(use-package company
  :delight
  :hook (prog-mode . company-mode)
  :init
  (defvar company-dabbrev-downcase nil))

(use-package company-go
  :config
  (add-to-list 'company-backends 'company-go))

(use-package company-irony
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-quickhelp
  :init
  (company-quickhelp-mode 1))

(use-package spray
  :init
  (setq spray-margin-top 6))
  ;; (setq spray-margin-left (- (/ (window-body-width) 2) 5))

;; go get -u github.com/alecthomas/gometalinter
;; gometalinter --install
(use-package flycheck-gometalinter
  :config
  (flycheck-gometalinter-setup)
  (setq flycheck-gometalinter-deadline "10s")
  (setq flycheck-gometalinter-fast t)) ; only run fast linters

(use-package ledger-mode ; requires ledger binary
  :mode "\\.ledger\\'"
  :hook (ledger-mode . flycheck-mode)
  :config
  (define-key ledger-mode-map (kbd "C-c c") 'ledger-mode-clean-buffer)
  (use-package flycheck-ledger))

(use-package slime
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  ;; (setq inferior-lisp-program "/usr/bin/clisp")
  (setq slime-contribs '(slime-fancy slime-company)))

(use-package slime-company)

(use-package yasnippet
  :delight yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-snippet-dirs
        '(yasnippet-snippets-dir
          "~/.emacs.d/snippets"))
  (yas-reload-all))

(use-package yasnippet-snippets)

(use-package smartparens
  :delight smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :init
  (setq sp-highlight-pair-overlay nil)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'prog-mode "{" nil :post-handlers '(("||\n[i]" "RET"))))

(use-package ivy
  :delight
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  :config
  (ivy-set-actions
   'counsel-find-file
   '(("x" find-file-other-window "other window")
     ("d" delete-file "delete"))))

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package smex) ; Used with counsel to get the recenently used commands

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this))

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-light t))

(use-package irony
  :hook (c-mode . irony-mode)
  :init
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :init
  (defun disable-elisp-flycheck ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-hook 'org-src-mode-hook 'disable-elisp-flycheck))

(use-package flycheck-irony
  :hook (flycheck-mode . flycheck-irony-setup))

(use-package magit)

(use-package markdown-mode)

(use-package browse-kill-ring)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package move-dup
  :bind
  ;; ("C-M-<up>" . md/duplicate-up)
  ;; ("C-M-<down>" . md/duplicate-down)
  ("M-p" . md/move-lines-up)
  ("M-n" . md/move-lines-down))

(use-package avy
  :bind ("C-r" . avy-goto-char-timer))

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 80))

(use-package ag)

(use-package delight) ; This is for using :delight with use-package as an optional dependency

;; Requires:
;; aspell
;; aspell-en
(use-package flyspell
  :delight
  :hook (text-mode . flyspell-mode)
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

(use-package imenu-list
  :init
  (setq imenu-list-auto-resize t))

;;; Load private config
(load "~/.emacs.d/lisp/private.el")

(provide 'init)
;;; init.el ends here
