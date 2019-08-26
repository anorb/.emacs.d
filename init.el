;;; init.el --- My Emacs config                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Austin Norberg

;; Author: Austin Norberg <austin@norberg.tech>

;;; Commentary:

;;

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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
      user-mail-address "austin@norb.xyz")

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

(setq desktop-save t)

(defvar an/desktop-save nil
  "Should Emacs save desktop when exiting?")

(add-hook 'desktop-after-read-hook
          (lambda () (setq an/desktop-save t)))

(advice-add 'desktop-save :around
            (lambda (fn &rest args)
              (if (bound-and-true-p an/desktop-save)
                  (apply fn args))))

(setq delete-by-moving-to-trash t
      sentence-end-double-space nil
      display-time-24hr-format t
      initial-scratch-message nil
      eshell-hist-ignoredups t)

;; Kills dired buffers on emacs close
(add-hook 'kill-emacs-hook #'an/kill-dired-buffers)

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
                                 (mode . js2-mode)
                                 (mode . rjsx-mode)))
		 ("emacs" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")
                           (name . "^\\*Help\\*$"))))))

;;; Load misc-defuns for keybinds
(when (file-exists-p "~/.emacs.d/lisp/misc-defuns.el")
  (load "~/.emacs.d/lisp/misc-defuns.el"))

;;; Custom keybinds
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [f5] 'revert-buffer)

(global-set-key (kbd "C-v") 'an/scroll-up-half)
(global-set-key (kbd "M-v") 'an/scroll-down-half)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-w") 'an/xah-cut-line-or-region)
(global-set-key (kbd "M-w") 'an/xah-copy-line-or-region)

(global-set-key (kbd "<C-backspace>") 'an/backward-delete-word)
(global-set-key (kbd "M-DEL") 'an/backward-delete-word)
(global-set-key (kbd "M-d") 'an/delete-word)

(global-set-key (kbd "C-x 2") 'an/split-window-below-focus)
(global-set-key (kbd "C-x 3") 'an/split-window-right-focus)

(global-set-key (kbd "H-e") 'eshell)

(global-set-key (kbd "C-x j") 'an/change-desktop)

(global-set-key (kbd "C-x d") 'dired)
(global-set-key (kbd "C-x C-d") 'dired-jump)
(global-set-key (kbd "C-x M-d") 'dired-other-window)

(global-set-key (kbd "C-x C-c") nil) ;; prevent save-buffers-kill-terminal keybind

;;; Built in packages
(use-package autorevert
  :delight auto-revert-mode
  :init
  (global-auto-revert-mode t))

(use-package subword
  :delight
  :init
  (global-subword-mode 1))

;;; MELPA packages
(use-package elfeed
  :bind (("C-x w" . elfeed)
  :map elfeed-search-mode-map
  ("C-c e" . an/hydra-elfeed/body))
  :init
  (setq elfeed-db-directory "~/.emacs.d/elfeed/elfeeddb")
  (setq-default elfeed-search-filter "@6-week-ago +unread")
  :config
  (defface elfeed-youtube
    '((t :inherit font-lock-constant-face))
    "Colors YouTube videos in Elfeed."
    :group 'elfeed)
  (defface elfeed-podcast
    '((t :inherit font-lock-doc-face))
    "Colors Podcasts in Elfeed."
    :group 'elfeed)
  (push '(youtube elfeed-youtube) elfeed-search-face-alist)
  (push '(podcast elfeed-podcast) elfeed-search-face-alist))

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

(use-package org
  :defer t
  :bind
  (("C-c c" . counsel-org-capture)
   ("C-c a" . org-agenda)
   :map org-mode-map
   ("H-i" . counsel-org-goto)
   ("H-u" . counsel-org-goto-all))
  :custom (org-modules '(org-tempo org-habit))
  :init
  (add-hook 'org-mode-hook #'(lambda ()
			       (visual-line-mode)
			       (org-indent-mode)))

  (defvar org-capture-bookmark nil)
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords '((type "TODO" "|" "DONE")))
  (setq org-hide-leading-stars t)
  (setq org-ellipsis "⤵")

  (setq org-directory "~/org/")
  (load-library "find-lisp")
  (setq org-agenda-files (find-lisp-find-files org-directory "\.org$"))

  (setq org-habit-show-all-today t)
  (setq org-habit-preceding-days 21)
  (setq org-habit-following-days 7)
  (setq org-habit-graph-column 50)

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-clock-display-default-range 'untilnow)
  (setq org-clock-mode-line-total 'today)

  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-cycle-separator-lines 0)

  (run-at-time "1 hour" 3600 'org-save-all-org-buffers) ; Save org-buffers every hour

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))

  (setq org-capture-templates
        `(("p" "Personal templates")
          ("pt" "tech task"     entry (file+headline ,(concat org-directory "personal.org") "Tech tasks")     "* TODO %?\n")
          ("pn" "non-tech task" entry (file+headline ,(concat org-directory "personal.org") "Non-tech tasks") "* TODO %?\n")
          ("pr" "research"      entry (file+headline ,(concat org-directory "personal.org") "Research notes") "* TODO %?\n")
          ("ps" "shopping list" entry (file+headline ,(concat org-directory "personal.org") "Shopping list")  "* %?\n")

          ("f" "finance task" entry (file+headline ,(concat org-directory "finance.org") "Tasks") "* TODO %?\n")
          ("i" "fitness task" entry (file+headline ,(concat org-directory "fitness.org") "Tasks") "* TODO %?\n")
          ("e" "emacs task"   entry (file+headline ,(concat org-directory "emacs.org")   "Tasks") "* TODO %?\n")

          ("b" "add book to reading list" entry (file+headline ,(concat org-directory "books.org")   "Reading list") "* READINGLIST %?\n")))
  (setq org-default-notes-file (concat org-directory "personal.org"))
  (setq org-archive-location (concat org-directory "archive/%s_archive::"))
  (setq org-src-window-setup 'other-window))

(use-package org-super-agenda
  :config
  (org-super-agenda-mode t)
  (setq org-super-agenda-groups '((:name "Schedule" :time-grid t)
                                  (:name "Habits" :habit t)
                                  (:name "Recurring" :tag "recurring")
                                  (:name "Today" :scheduled today)
                                  (:name "Due today" :deadline today)
                                  (:name "Overdue" :scheduled past)
                                  (:name "Due soon" :deadline future)
                                  (:name "Dates" :category "people")))

  (setq org-agenda-custom-commands
        '(("t" "All TODOs groups by category" todo ""
           ((org-super-agenda-groups
             '((:discard (:and (:not (:todo "TODO")) :category "people" :tag "recurring"))
               (:auto-category t)))))
          ("b" "View reading list" todo ""
           ((org-super-agenda-groups
             '((:discard (:not (:and (:not (:todo ("TODO")) :category "books"))))
               (:auto-parent t))))))))

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
;; go get github.com/stamblerre/gocode
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

(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-cycle)))

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

(use-package ledger-mode ; requires ledger binary
  :mode "\\.ledger\\'"
  :hook (ledger-mode . flycheck-mode)
  :bind (:map ledger-mode-map ("C-c c" . 'an/hydra-ledger/body))
  :config
  (use-package flycheck-ledger)
  (setq ledger-reports
        '(("this year" "%(binary) -f %(ledger-file) bal --period \"this year\" ^Expenses ^Income --invert")
          ("last year" "%(binary) -f %(ledger-file) bal --period \"last year\" ^Expenses ^Income --invert")
          ("bal"       "%(binary) -E -f %(ledger-file) bal")
          ("reg"       "%(binary) -f %(ledger-file) reg")
          ("payee"     "%(binary) -f %(ledger-file) reg @%(payee)")
          ("account"   "%(binary) -f %(ledger-file) reg %(account)"))))

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
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all))

(use-package smartparens
  :delight smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :bind (:map smartparens-mode-map
              ("C-H-f" . sp-forward-sexp)
	      ("C-H-b" . sp-backward-sexp)
              ("C-<right>" . sp-forward-slurp-sexp)
	      ("C-<left>" . sp-forward-barf-sexp)
	      ("C-<up>" . sp-backward-slurp-sexp)
	      ("C-<down>" . sp-backward-barf-sexp))
  :init
  (setq sp-highlight-pair-overlay nil)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'prog-mode "{" nil :post-handlers '(("||\n[i]" "RET"))))

(use-package expand-region
  :bind ("M-@" . er/expand-region))

(use-package ivy
  :delight
  :init
  (ivy-mode 1)
  :config
  (setq ivy-count-format "%d/%d "
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("H-i" . counsel-imenu)
  ("H-s" . counsel-ag)
  :config
  (ivy-set-actions
   'counsel-find-file
   '(("x" find-file-other-window "other window")
     ("d" delete-file "delete"))))

(use-package swiper
  :bind
  ("C-s" . swiper-isearch))

(use-package smex) ; Used with counsel to get the recenently used commands

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this))

(use-package solarized-theme
  :defer t
  :init
  (setq solarized-scale-org-headlines nil)
  (setq solarized-high-contrast-mode-line t))

(use-package gruvbox-theme
  :defer t
  :init
  (load-theme 'gruvbox-light-hard))

(use-package irony
  :hook (c-mode . irony-mode)
  :init
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :bind ("C-H-c" . flycheck-buffer)
  :init
  (defun disable-elisp-flycheck ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-hook 'org-src-mode-hook 'disable-elisp-flycheck)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package flycheck-irony
  :hook (flycheck-mode . flycheck-irony-setup))

(use-package magit)

(use-package markdown-mode)

(use-package browse-kill-ring
  :bind
  ("M-y" . browse-kill-ring))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package move-dup)

(use-package avy
  :bind ("C-r" . avy-goto-char-timer))

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 80))

(use-package ag)
(use-package delight) ; This is for using :delight with use-package as an optional dependency
(use-package lua-mode)
(use-package ivy-pass)
(use-package rainbow-mode)

;; Requires:
;; aspell
;; aspell-en
(use-package flyspell
  :delight
  :hook (text-mode . flyspell-mode)
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

;; Requires:
;; wordnet
(use-package synosaurus
  :config
  (setq synosaurus-backend 'synosaurus-backend-wordnet)
  (setq synosaurus-choose-method 'default))

;; Requires:
;; wordnet
(use-package wordnut)

(use-package speed-type)

(use-package hydra
  :defer t
  :bind (("C-c o" . an/hydra-org/body)
         ("C-c m" . an/hydra-move-dup/body)
         ("C-c s" . an/hydra-spell/body)
         ("C-c g" . an/hydra-go-to-file/body))
  :init
  (defhydra an/hydra-org (:hint nil)
    "
^Clock^                    ^Capture^           ^Search
-----------------------------------------------------
_i_ clock in               _c_: capture        _g_: goto header in file
_o_ clock out              _L_: last stored    _G_: goto header in all
_j_ goto current clock
_l_ goto last clock
_t_ toggle mode line total
_d_ display time
^ ^
"
    ;; Clock
    ("i" org-clock-in :exit t)
    ("o" org-clock-out :exit t)
    ("j" org-clock-goto)
    ("l" org-clock-in-last :exit t)
    ("t" toggle-org-clock-mode-line-total-setting)
    ("d" org-clock-display)
    ;; Capture
    ("c" counsel-org-capture)
    ("L" org-capture-goto-last-stored)
    ;; Search
    ("g" counsel-org-goto)
    ("G" counsel-org-goto-all)
    ;; Misc
    ("q" nil "cancel"))

  (defhydra an/hydra-elfeed ()
    "elfeed"
    ("d" (elfeed-search-set-filter "@6-weeks-ago +unread") "Default")
    ("p" (elfeed-search-set-filter "+podcast +unread") "Podcasts")
    ("o" an/elfeed-visit-maybe-externally "Open" :exit t)
    ("D" an/elfeed-download-media "Download" :exit t)
    ("q" nil "quit"))

  (defhydra an/hydra-move-dup ()
    "Move/dup"
    ("u" md-move-lines-up "move up")
    ("d" md-move-lines-down "move down")
    ("U" md-duplicate-up "duplicate up")
    ("D" md-duplicate-down "duplicate down"))

  ;; Following hydras inspired by...
  ;; https://github.com/rememberYou/.emacs.d/blob/master/config.org#hydra
  (defhydra an/hydra-spell ()
    "Spelling"
    ("t" synosaurus-choose-and-replace "Find synonym and replace" :exit t)
    ("s" wordnut-lookup-current-word "Lookup current word" :exit t))

  (defhydra an/hydra-ledger ()
    "Ledger"
    ("b" leadger-add-transaction "add" :exit t)
    ("c" ledger-mode-clean-buffer "clear" :exit t)
    ("i" ledger-copy-transaction-at-point "copy" :exit t)
    ("s" ledger-delete-current-transaction "delete" :exit t)
    ("r" ledger-report "report" :exit t))

  (defhydra an/hydra-go-to-file ()
    "Go to"
    ("e" (find-file "~/.emacs.d/init.el") "init.el" :exit t)
    ("b" (find-file "~/.bashrc") ".bashrc" :exit t)))

;;; Local packages
;; These are packages not available on MELPA and/or have been modified
(use-package web-server
  :ensure nil
  :load-path "lisp/web-server")

;;; Load private config
(when (file-exists-p "~/.emacs.d/lisp/private.el")
  (load "~/.emacs.d/lisp/private.el"))

(provide 'init)
;;; init.el ends here
