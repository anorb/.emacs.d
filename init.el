;;; init.el --- My Emacs config                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Austin Norberg

;; Author: Austin Norberg <austin@norberg.tech>

;;; Commentary:

;;

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

;; Load separately stored custom file
(setq custom-file "~/.config/emacs/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq user-full-name "Austin Norberg"
      user-mail-address "austin@norb.xyz")

(setq frame-title-format "%b - Emacs"
      icon-title-format "%b - Emacs")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))))


(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

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
(setq ad-redefinition-action 'accept)

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
(setq backup-directory-alist '(("." . "~/.config/emacs/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 6    ; how many of the newest versions to keep
      kept-old-versions 2    ; and how many of the old
      create-lockfiles nil)

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
(when (file-exists-p "~/.config/emacs/lisp/misc-defuns.el")
  (load "~/.config/emacs/lisp/misc-defuns.el"))

;;; Custom keybinds
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [f5] 'revert-buffer)

(global-set-key (kbd "C-v") 'an/scroll-up-half)
(global-set-key (kbd "M-v") 'an/scroll-down-half)

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

(use-package rmail
  :init
  (setq rmail-file-name "~/Documents/system-mail"))

(use-package browse-url
  :init
  (setq browse-url-browser-function '(("file://" . eww-browse-url)
                                      ("." . browse-url-default-browser))))
(use-package url-cache
  :ensure nil
  :init
  (setq url-cache-directory "/tmp/url/cache/"))

(use-package winner
  :init
  (winner-mode))

(use-package tab-bar
  :bind
  ("H-t" . an/hydra-window-management/body)
  ;; monkey-with-hammer.png
  ("M-1" .  (lambda () (interactive) (tab-bar-select-tab 1)))
  ("M-2" .  (lambda () (interactive) (tab-bar-select-tab 2)))
  ("M-3" .  (lambda () (interactive) (tab-bar-select-tab 3)))
  ("M-4" .  (lambda () (interactive) (tab-bar-select-tab 4)))
  ("M-5" .  (lambda () (interactive) (tab-bar-select-tab 5)))
  ("M-6" .  (lambda () (interactive) (tab-bar-select-tab 6)))
  ("M-7" .  (lambda () (interactive) (tab-bar-select-tab 7)))
  ("M-8" .  (lambda () (interactive) (tab-bar-select-tab 8)))
  ("M-9" .  (lambda () (interactive) (tab-bar-select-tab 9)))
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-tab-to 'rightmost))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("H-k" . an/hydra-flymake/body))
  :init
  (setq flymake-suppress-zero-counters t))

;; Requires:
;; aspell
;; aspell-en
(use-package flyspell
  :delight
  :hook (text-mode . flyspell-mode)
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--personal=.config/aspell/.aspell.en.pws")))

(use-package time
  :init
  (setq display-time-24hr-format t))

;;; MELPA packages
(use-package delight)

(use-package org-plus-contrib
  :defer t
  :bind
  (("C-c c" . counsel-org-capture)
   ("C-c a" . org-agenda)
   :map org-mode-map
   ("H-i" . counsel-org-goto)
   ("H-u" . counsel-org-goto-all))
  :custom (org-modules '(org-tempo org-habit org-checklist))
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

          ("b" "add book to reading list" entry (file+headline ,(concat org-directory "books.org")   "Reading list") "* READINGLIST %^{Title}\n:PROPERTIES:\n:AUTHOR: %^{Author}\n:GENRE: %^{Genre}\n:PAGES: %^{Pages}\n:END:\n")))

  (setq org-link-abbrev-alist
        '(("ddg"  . "https://duckduckgo.com/?q=%s")
          ("wiki" . "https://en.wikipedia.org/w/index.php?search=%s")))
  (org-link-set-parameters "epiphany" :follow (lambda (path) (browse-url-epiphany path)))
  (org-link-set-parameters "eww" :follow (lambda (path) (eww path)))
  (setq org-default-notes-file (concat org-directory "personal.org"))
  (setq org-archive-location (concat org-directory "archive/%s_archive::"))
  (setq org-src-window-setup 'other-window))

(use-package org-super-agenda
  :config
  (org-super-agenda-mode t)
  (setq org-super-agenda-groups '((:name "Schedule" :time-grid t)
                                  (:name "Habits" :habit t)
                                  (:name "Recurring" :tag "recurring")
                                  (:discard  (:tag "drill"))
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
               (:auto-parent t)))))
          ("f" "Flash cards" tags "drill"
           ((org-super-agenda-groups
             '((:discard (:not (:scheduled today :scheduled past)))
               (:auto-parent t))))))))

(use-package elfeed
  :bind (("C-x w" . elfeed)
  :map elfeed-search-mode-map
  ("C-c e" . an/hydra-elfeed/body)
  ("o" . an/elfeed-visit-maybe-externally))
  :init
  (setq elfeed-db-directory "~/.config/emacs/elfeed/elfeeddb")
  (setq-default elfeed-search-filter "@6-week-ago +unread")
  (setq elfeed-show-entry-switch 'display-buffer)
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

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (ace-window-display-mode 1)
  (setq aw-background nil))

(use-package git-gutter
  :delight
  :init
  (global-git-gutter-mode 1))

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

(use-package eldoc
  :delight)

;; To get the latest version of gopls:
;; go get golang.org/x/tools/gopls@latest
(use-package eglot
  :hook
  (go-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  :bind (:map eglot-mode-map ("H-l" . 'an/hydra-eglot/body)))

(use-package go-mode
  :init
  (setq gofmt-command "goimports")
  (defun go-hooks ()
    (add-hook 'before-save-hook #'eglot-format-buffer)
    (add-hook 'before-save-hook #'gofmt-before-save))
  (add-hook 'go-mode-hook #'go-hooks))

(use-package go-playground
  :init
  (setq go-playground-basedir "~/Projects/go/src/playground"))

(use-package company
  :delight
  :hook (prog-mode . company-mode)
  :init
  (setq completion-ignore-case t)
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-idle-delay 0))

(use-package ledger-mode ; requires ledger binary
  :mode "\\.ledger\\'"
  :hook (ledger-mode . ledger-flymake-enable)
  :bind (:map ledger-mode-map ("C-c c" . 'an/hydra-ledger/body))
  :config
  (add-hook 'ledger-mode-hook (lambda ()
                                (setq-local tab-always-indent 'complete)
                                (setq-local completion-ignore-case t)
                                (setq-local ledger-complete-in-steps t)))
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
  (setq slime-contribs '(slime-fancy slime-company))
  (setq common-lisp-hyperspec-root (concat "file://" (expand-file-name "~/Projects/lisp/HyperSpec/"))))

(use-package slime-company)

(use-package yasnippet
  :delight yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
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

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)))

(use-package expand-region
  :bind ("H-w" . er/expand-region))

(use-package ivy
  :delight
  :init
  (ivy-mode 1)
  (setq ivy-count-format "%d/%d "
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil))

(use-package swiper
  :bind
  ("C-s" . swiper-isearch))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("H-i" . counsel-imenu)
  ("H-s" . counsel-ag)
  ("C-h o" . counsel-describe-symbol)
  ("C-h v" . counsel-describe-variable)
  ("C-h f" . counsel-describe-function)
  :config
  (ivy-set-actions
   'counsel-find-file
   '(("x" find-file-other-window "other window")
     ("d" delete-file "delete")))
  ;; Couldn't find a better way to set initial input for ivy so we'll try this
  (add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . "")))

(use-package amx
  :config
  (amx-mode 1))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev))

(use-package flyspell-correct-ivy
    :bind ("C-M-;" . flyspell-correct-wrapper)
    :init
    (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this))

(use-package modus-operandi-theme
  :init
  (load-theme 'modus-operandi))

(use-package modus-vivendi-theme)

(use-package magit)

(use-package markdown-mode)

(use-package browse-kill-ring
  :bind
  ("M-y" . browse-kill-ring))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package avy
  :bind ("C-r" . avy-goto-char-timer))

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 80))

(use-package ivy-pass)
(use-package restclient)
(use-package elpher)

;; Requires:
;; wordnet
(use-package synosaurus
  :config
  (setq synosaurus-backend 'synosaurus-backend-wordnet)
  (setq synosaurus-choose-method 'default))

;; Requires:
;; wordnet
(use-package wordnut)

(use-package vterm
  :bind ("C-`" . vterm)
  :hook
  (vterm-mode . vterm-hook)
  :init
  (setq vterm-kill-buffer-on-exit t)
  (defun vterm-hook ()
    (setq-local global-hl-line-mode nil)
    (setq-local truncate-lines t)))

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
    ("d" (elfeed-search-clear-filter) "Default")
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
    ("a" ledger-add-transaction "add" :exit t)
    ("c" ledger-copy-transaction-at-point "copy" :exit t)
    ("C" ledger-mode-clean-buffer "clear" :exit t)
    ("d" ledger-delete-current-transaction "delete" :exit t)
    ("r" ledger-report "report" :exit t))

  (defhydra an/hydra-go-to-file ()
    "Go to"
    ("e" (find-file "~/.config/emacs/init.el") "init.el" :exit t)
    ("b" (find-file "~/.bashrc") ".bashrc" :exit t))

  (defhydra an/hydra-dumb-jump (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))

  (defhydra an/hydra-flymake (:color blue :columns 3)
    "Flymake"
    ("n" flymake-goto-next-error "Next error")
    ("p" flymake-goto-prev-error "Previous error")
    ("d" flymake-show-diagnostics-buffer "Show diagnostic buffer")
    ("s" flymake-start "Start")
    ("q" nil "Quit"))

  (defhydra an/hydra-window-management (:hint nil)
    "
^tab-bar^           ^winner^
-------------------------------------
_t_ switch tabs     _U_: undo
_n_ new tab         _R_: redo
_r_ rename tab
_m_ move tab
_k_ close tab
^ ^
"
    ;; tab-bar
    ("t" tab-bar-switch-to-tab :exit t)
    ("n" tab-bar-new-tab)
    ("r" tab-bar-rename-tab)
    ("m" tab-bar-move-tab)
    ("k" tab-bar-close-tab)
    ;; winner
    ("U" winner-undo)
    ("R" winner-redo)
    ;; Misc
    ("q" nil "cancel"))

  (defhydra an/hydra-eglot (:color blue :columns 3)
  "Eglot"
  ("x" eglot "eglot" :exit t)
  ("r" eglot-rename "rename" :exit t)
  ("f" eglot-format "format buffer" :exit t)))

;;; Local packages
;; These are packages not available on MELPA and/or have been modified
(use-package web-server
  :ensure nil
  :load-path "lisp/web-server")

(use-package org-archive-subtree-hierarchical
  :ensure nil
  :load-path "lisp/org-archive-subtree-hierarchical")

(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :init
  (setq mu4e-change-filenames-when-moving t
        mu4e-headers-skip-duplicates t
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        mu4e-get-mail-command  "mbsync -a"
        mu4e-attachments-dir "~/Downloads"
        mu4e-compose-signature-auto-include nil)

  ;; Mail directories
  (setq mu4e-refile-folder "/Archive"
        mu4e-sent-folder   "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder  "/Trash")

  ;; SMTP setup
  (setq message-send-mail-function   'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-server         "smtp.fastmail.com"
        smtpmail-smtp-service        465
        smtpmail-stream-type         'ssl)

  :config
  (fset 'my-move-to-trash "mTrash")
  (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
  (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)
  (setq mu4e-bookmarks
        '(( :name  "Inbox"
                   :query "maildir:/Inbox"
                   :key ?i)
          ( :name  "Unread messages"
                   :query "flag:unread AND NOT maildir:/Trash"
                   :key ?u)
          ( :name "Today's messages"
                  :query "date:today..now"
                  :key ?t)
          ( :name "Last 7 days"
                  :query "date:7d..now"
                  :hide-unread t
                  :key ?w)
          ( :name "Messages with images"
                  :query "mime:image/*"
                  :key ?p))))

;;; Load private config
(when (file-exists-p "~/.config/emacs/lisp/private.el")
  (load "~/.config/emacs/lisp/private.el"))

(provide 'init)
;;; init.el ends here
