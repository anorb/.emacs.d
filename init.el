;;; init.el --- My Emacs config                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Austin Norberg

;; Author: Austin Norberg <austin@norberg.tech>

;;; Commentary:

;;

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Load separately stored custom file
(setq custom-file "~/.config/emacs/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; use-package settings
(defvar use-package-always-ensure t)
(defvar use-package-enable-imenu-support t)

(package-initialize)

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
      initial-scratch-message nil)

;; Kills dired buffers on emacs close
(add-hook 'kill-emacs-hook #'an/kill-dired-buffers)

;;; Load misc-defuns for keybinds
(when (file-exists-p "~/.config/emacs/lisp/misc-defuns.el")
  (load "~/.config/emacs/lisp/misc-defuns.el"))

;;; Custom keybinds
(global-set-key [f5] 'revert-buffer)

(global-set-key (kbd "C-v") 'an/scroll-up-half)
(global-set-key (kbd "M-v") 'an/scroll-down-half)

(global-set-key (kbd "C-w") 'an/xah-cut-line-or-region)
(global-set-key (kbd "M-w") 'an/xah-copy-line-or-region)

(global-set-key [f7] 'an/frame-recenter)

(global-set-key (kbd "<C-backspace>") 'an/backward-delete-word)
(global-set-key (kbd "M-DEL") 'an/backward-delete-word)
(global-set-key (kbd "M-d") 'an/delete-word)

(global-set-key (kbd "C-x 2") 'an/split-window-below-focus)
(global-set-key (kbd "C-x 3") 'an/split-window-right-focus)

(global-set-key (kbd "C-x j") 'an/change-desktop)

(unbind-key "C-x C-c") ;; prevent save-buffers-kill-terminal keybind
(unbind-key "C-z")     ;; prevent suspend
(unbind-key "C-x C-z") ;; prevent minimize

;;; Built in packages
(use-package emacs
  :config
  (setq make-backup-files nil  ; Prevent backups from being created
        backup-directory-alist '(("." . "~/.config/emacs/backup"))
        backup-by-copying t    ; Don't delink hardlinks
        version-control t      ; Use version numbers on backups
        delete-old-versions t  ; Automatically delete excess backups
        kept-new-versions 6    ; how many of the newest versions to keep
        kept-old-versions 2)   ; and how many of the old
  (setq auto-save-default t)
  (setq create-lockfiles nil)

  (load-theme 'modus-operandi)
  (define-key global-map (kbd "<f6>") #'modus-themes-toggle))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :init
  ;; Ibuffer changes
  (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-auto-mode 1)
	      (ibuffer-switch-to-saved-filter-groups "default")))

  (setq-default ibuffer-saved-filter-groups
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
                 (name . "^\\*Help\\*$")))))))

(use-package dired
  :bind (("C-x d" . dired)
         ("C-x C-d" . dired-jump)
         ("C-x M-d" . dired-other-window)))

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
  (setq flymake-suppress-zero-counters nil)
  (setq flymake-mode-line-format
      '("" flymake-mode-line-exception flymake-mode-line-counters))

(setq flymake-mode-line-counter-format
        '(" [" flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter "]")))

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

;; GPG stuff
(use-package epg-config
  :ensure nil
  :init
  (setq epg-gpg-program "gpg2")
  (setq epg-gpg-home-directory "~/.gnupg"))

(use-package eshell
  :bind (("C-`" . eshell)))

(use-package desktop
  :init
  ;; Save buffers & window config on exit
  (desktop-save-mode 1)
  (setq desktop-save t))

(use-package advice
  :ensure nil
  :init
  (setq ad-redefinition-action 'accept))

(use-package savehist
  :init
  (savehist-mode))

(use-package eglot
  :hook
  (go-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  :bind (:map eglot-mode-map ("H-l" . 'an/hydra-eglot/body)))

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ("C-SPC" . consult-line))
  :init
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq isearch-repeat-on-direction-change t)
  (setq isearch-wrap-pause 'no-ding))

(use-package pixel-scroll
  :ensure nil
  :init
  (pixel-scroll-precision-mode))

(use-package dictionary)

(use-package windmove
  :init
  (windmove-default-keybindings 'control))

(use-package dbus
  :init
  (defun theme-switcher (value)
     (pcase value
          ;; No Preference
       (0 ;;(shell-command "gsettings set org.gnome.desktop.interface gtk-theme \"adw-gtk3\"")
          (load-theme 'modus-operandi t))
           ;; Prefers dark
       (1 ;;(shell-command "gsettings set org.gnome.desktop.interface gtk-theme \"adw-gtk3-dark\"")
          (load-theme 'modus-vivendi t))
           ;; Prefers light. Not currently used by Gnome
       (2 ;;(shell-command "gsettings set org.gnome.desktop.interface gtk-theme \"adw-gtk3\"")
         (load-theme 'modus-operandi t))
           (_ (message "Invalid key value"))))

  (defun handler (value)
    (theme-switcher (car (car value))))

  (defun signal-handler (namespace key value)
    (if (and
         (string-equal namespace "org.freedesktop.appearance")
         (string-equal key "color-scheme"))
        (theme-switcher (car value))))
  :config
  (dbus-call-method-asynchronously
   :session
   "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "Read"
   #'handler
   "org.freedesktop.appearance"
   "color-scheme")

  (dbus-register-signal
   :session
   "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "SettingChanged"
   #'signal-handler))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; MELPA packages
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package delight)

(use-package consult
  :bind (("M-g i" . consult-imenu)
         ("H-s" . consult-grep)
         ("C-x b" . consult-buffer))
  :init
  (setq consult-line-start-from-top t))

(use-package marginalia
  :init
  (marginalia-mode))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  (setq completion-styles '(orderless basic)))

(use-package orderless)

(use-package corfu
  :hook ((prog-mode . corfu-mode))
  :init
  (setq corfu-auto t))

(use-package org
  :defer t
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   :map org-mode-map
   ("H-i" . consult-org-heading)
   :map org-agenda-keymap
   ("C-c C-t" . an/org-mark-done))
  :custom (org-modules '(org-tempo org-habit org-checklist org-mouse))
  :init
  (setq org-directory "~/Documents/org/")
  (load-library "find-lisp")
  (setq org-agenda-files (find-lisp-find-files org-directory "\.org$"))
  (run-at-time "1 hour" 3600 'org-save-all-org-buffers) ; Save org-buffers every hour
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))

  (add-hook 'org-mode-hook #'(lambda ()
			       (visual-line-mode)
			       (org-indent-mode)))

  (setq org-agenda-custom-commands
      '(("d" "Day agenda"
         ((agenda ""
                  ((org-agenda-span 5)
                   (org-agenda-sorting-strategy '(habit-up time-up scheduled-down category-keep))))
          (todo "NEXT" ((org-agenda-overriding-header "\nIn progress:")))
          (todo "WAITING" ((org-agenda-overriding-header "\nOn hold:"))))
         ((org-agenda-compact-blocks t)))
        ("A" "All TODOs, sorted by tag"
         ((todo "TODO" ((org-agenda-sorting-strategy '(tag-up))))))
        ("I" . "Inbox queries")
        ("Is" "Tag - shopping"
         ((tags "shopping"))
         ((org-agenda-files '("~/Documents/org/inbox.org"))))
        ("Iz" "Tag - zettel"
         ((tags "zettel"))
         ((org-agenda-files '("~/Documents/org/inbox.org"))))
        ("b" "View reading list"
         ((todo "STARTED" ((org-agenda-files '("~/Documents/org/books.org"))
                           (org-agenda-overriding-header "\nBooks started:\n------------------\n")))
          (todo "READINGLIST")
          (todo "STARTED")
          (todo "NEEDSREVIEW")
          (todo "READ"))
         ((org-agenda-files '("~/Documents/org/books.org"))))))

  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (setq org-hide-leading-stars t)
  (setq org-ellipsis "⤵")
  (setq org-startup-folded t)

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-cycle-separator-lines 0)

  (setq org-link-abbrev-alist
        '(("ddg"  . "https://duckduckgo.com/?q=%s")
          ("wiki" . "https://en.wikipedia.org/w/index.php?search=%s")))
  (org-link-set-parameters "epiphany" :follow (lambda (path) (browse-url-epiphany path)))
  (org-link-set-parameters "eww" :follow (lambda (path) (eww path)))
  (setq org-default-notes-file (concat org-directory "personal.org"))
  (setq org-archive-location (concat org-directory "archive/%s_archive::"))
  (setq org-src-window-setup 'other-window)

  (use-package org-duration
    :ensure nil
    :init
    (setq org-duration-format 'h:mm))

  (use-package org-habit
    :ensure nil
    :init
    (setq org-habit-show-all-today t)
    (setq org-habit-preceding-days 21)
    (setq org-habit-following-days 7)
    (setq org-habit-graph-column 50))

  (use-package org-clock
    :ensure nil
    :init
    (setq org-clock-display-default-range 'untilnow)
    (setq org-clock-mode-line-total 'today))

  (use-package org-capture
    :ensure nil
    :init
    (setq org-capture-bookmark nil)
    (setq org-capture-templates
          `(("t" "task"     entry (file+headline ,(concat org-directory "personal.org") "Tasks")     "* TODO %? %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines-after 1)
            ("i" "inbox" entry (file ,(concat org-directory "inbox.org"))  "* %?\n")
            ("s" "shopping list" entry (file ,(concat org-directory "inbox.org")) "* %? :shopping:\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines-after 1)
            ("b" "add book to reading list" entry (file+headline ,(concat org-directory "books.org")   "Reading list") "* READINGLIST %^{Title}\n:PROPERTIES:\n:AUTHOR: %^{Author}\n:GENRE: %^{Genre}\n:PAGES: %^{Pages}\n:END:\n")))))

(use-package org-caldav
  :ensure t
  :defer
  :config
  (setq org-caldav-url an/caldav-url)
  (setq org-caldav-calendars
        `((:calendar-id ,an/caldav-calendar-id
                        :inbox "~/Documents/org/schedule.org")))
  (setq org-caldav-files nil)
  (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
        org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start)
        org-icalendar-include-todo t
        org-icalendar-timezone "America/Chicago"))

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
  (defface elfeed-video
    '((t :inherit font-lock-constant-face))
    "Colors Videos in Elfeed."
    :group 'elfeed)
  (defface elfeed-podcast
    '((t :inherit font-lock-doc-face))
    "Colors Podcasts in Elfeed."
    :group 'elfeed)
  (push '(video elfeed-video) elfeed-search-face-alist)
  (push '(podcast elfeed-podcast) elfeed-search-face-alist))

(use-package undo-tree
  :delight
  :bind
  ("<f9>" . 'undo)
  ("<f10>" . 'redo)
  :init
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

(use-package diff-hl
  :delight
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

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
;; go install golang.org/x/tools/gopls@latest
(use-package go-mode
  :init
  (defun +eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))
  (defun go-hooks ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'+eglot-organize-imports))
  (add-hook 'go-mode-hook #'go-hooks))

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
          ("this month" "%(binary) -f %(ledger-file) bal --period \"this month\" ^Expenses ^Income --invert")
          ("last month" "%(binary) -f %(ledger-file) bal --period \"last month\" ^Expenses ^Income --invert")
          ("bal"       "%(binary) -E -f %(ledger-file) bal")
          ("reg"       "%(binary) -f %(ledger-file) reg")
          ("payee"     "%(binary) -f %(ledger-file) reg @%(payee)")
          ("account"   "%(binary) -f %(ledger-file) reg %(account)"))))

(use-package slime
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy slime-company))
  (setq common-lisp-hyperspec-root (concat "file://" (expand-file-name "~/Projects/lisp/HyperSpec/"))))

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
	      ("C-H-b" . sp-backward-sexp))
  :init
  (setq sp-highlight-pair-overlay nil)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'prog-mode "{" nil :post-handlers '(("||\n[i]" "RET"))))

(use-package hl-todo
  :bind (:map hl-todo-mode-map
              ("C-c C-t" . an/hydra-hl-todo/body))
  :hook ((prog-mode . hl-todo-mode)))

(use-package expand-region
  :bind ("H-w" . er/expand-region))

(use-package magit
  :bind
  (:map magit-mode-map
        ("M-1" . nil)
        ("M-2" . nil)
        ("M-3" . nil)
        ("M-4" . nil))
  :init
  (setq magit-save-repository-buffers 'dontask))

(use-package yaml-mode)

(use-package markdown-mode)

(use-package browse-kill-ring
  :bind
  ("M-y" . browse-kill-ring))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package avy
  :bind ("H-a" . avy-goto-char-timer))

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 80))

(use-package restclient)

(use-package rec-mode)

(use-package denote
  :bind
  ("C-c n" . an/hydra-denote/body)
  :init
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-known-keywords '("emacs"))
  (setq denote-date-prompt-use-org-read-date t))

(use-package hydra
  :defer t
  :bind (("C-c o" . an/hydra-org/body)
         ("C-c m" . an/hydra-move-dup/body)
         ("C-c s" . an/hydra-spell/body)
         ("C-c g" . an/hydra-go-to-file/body))
  :init
  (defhydra an/hydra-org (:hint nil)
    "
^Clock^                    ^Capture^           ^Search                   ^Misc
-----------------------------------------------------------------------------------------
_i_ clock in               _c_: capture        _g_: goto header in file  _s_ caldav sync
_o_ clock out              _L_: last stored                              _n_ narrow
_j_ goto current clock
_l_ goto last clock
_t_ toggle mode line total
_d_ display time
_r_ clock report
^ ^
"
    ;; Clock
    ("i" org-clock-in :exit t)
    ("o" org-clock-out :exit t)
    ("j" org-clock-goto)
    ("l" org-clock-in-last :exit t)
    ("t" toggle-org-clock-mode-line-total-setting)
    ("d" org-clock-display)
    ("r" org-clock-report :exit t)
    ;; Capture
    ("c" org-capture)
    ("L" org-capture-goto-last-stored)
    ;; Search
    ("g" consult-org-heading)
    ;; Misc
    ("s" org-caldav-sync)
    ("n" org-toggle-narrow-to-subtree :exit t)
    ;; Jump to schedule??
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
_f_ next tab        _U_: undo
_p_ prior tab       _R_: redo
_t_ switch tabs
_n_ new tab
_r_ rename tab
_m_ move tab
_k_ close tab
^ ^
"
    ;; tab-bar
    ("f" tab-bar-switch-to-next-tab)
    ("p" tab-bar-switch-to-prev-tab)
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
  ("f" eglot-format "format buffer" :exit t))

  (defhydra an/hydra-hl-todo (:color blue :columns 2)
    "hl-todo"
    ("n" hl-todo-next "next")
    ("p" hl-todo-previous "previous")
    ("o" hl-todo-occur "occur" :exit t)
    ("i" hl-todo-insert "insert" :exit t))

  (defhydra an/hydra-denote ()
    "denote"
    ("n" denote "New note" :exit t)
    ("d" denote-date "New note (by date)" :exit t)
    ("l" denote-link "Link to note" :exit t)
    ("r" denote-rename-file-using-front-matter "Rename" :exit t)
    ("D" (dired denote-directory) "Denote dired" :exit t)
    ("q" nil "quit")))

;;; Local packages
;; These are packages not available on MELPA and/or have been modified
(use-package web-server
  :pin manual
  :load-path "lisp/web-server")

(use-package org-archive-subtree-hierarchical
  :pin manual
  :load-path "lisp/org-archive-subtree-hierarchical")

(use-package ob-rec
  :pin manual
  :load-path "lisp/ob-rec")

(use-package mu4e
  :pin manual
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :init
  (setq mu4e-change-filenames-when-moving t
        mu4e-headers-skip-duplicates t
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        mu4e-get-mail-command  "mbsync -a"
        mu4e-attachment-dir "~/Downloads"
        mu4e-compose-signature-auto-include nil
        mu4e-compose-format-flowed t)

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
