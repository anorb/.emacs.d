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

;; Code formatting
(setq tab-width 4)
(setq c-default-style "k&r")
(setq c-basic-offset 2)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)

;; GPG stuff
(setq epg-gpg-program "gpg2")
(setq epg-gpg-home-directory "~/.gnupg")

;; Tramp stuff
(setq tramp-default-method "ssh")

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
(setq use-package-always-ensure t)

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
(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-saved-filter-groups
	'(("default"
		 ("dired" (mode . dired-mode))
		 ("org" (name . "^.*org$"))
		 ;; ("web" (or (mode . web-mode) (mode . js2-mode)))
		 ("shell" (or (mode . eshell-mode)
                              (mode . shell-mode)))
		 ("programming" (or
                                 (mode . sh-mode)
                                 (mode . lisp-mode)
                                 (mode . elisp-mode)
                                 (mode . go-mode)
                                 (mode . js2-mode)))
		 ("emacs" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")
                           (name . "^\\*Help\\*$"))))))




;;; Custom keybinds
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-v") 'scroll-up-half)
(global-set-key (kbd "C-v") 'scroll-down-half)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-w") 'xah-cut-line-or-region)
(global-set-key (kbd "M-w") 'xah-copy-line-or-region)

(global-set-key (kbd "C-r") nil) ; Unset C-r since I don't use I-search

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
  :bind ("C-x w" . elfeed)
  :init
  (setq elfeed-db-directory "~/Sync/elfeed/elfeeddb")
  (setq-default elfeed-search-filter "@2-week-ago +unread -star")
  :config
  (defalias 'elfeed-toggle-star
	(elfeed-expose #'elfeed-search-toggle-all 'star))
  (define-key elfeed-search-mode-map (kbd "l") 'elfeed-toggle-star))

(use-package elfeed-org
  :init
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(use-package autorevert
  :init
  (global-auto-revert-mode t)
  :delight auto-revert-mode)

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :delight
  :bind
  ("<f9>" . 'undo)
  ("<f10>" . 'redo))

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
  (setq org-completion-use-ido t)
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords '((type "TODO" "|" "DONE")))
  (setq org-hide-leading-stars t)
  (setq org-ellipsis "⤵")

  (setq org-directory "~/Sync/org/")
  (load-library "find-lisp")
  (setq org-agenda-files (find-lisp-find-files org-directory "\.org$"))

  (setq org-capture-templates
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
  :mode "\\.js\\'"
  :init
  ;; Turn off js2 mode errors & warnings
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  :config
  (setq js-indent-level 2)

  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (setq company-tooltip-align-annotations t)   ; aligns annotation to the right hand side
  (add-hook 'js2-mode-hook #'setup-tide-mode))

(use-package web-mode
  :mode ("\\.hbs\\'" "\\.htm\\'" "\\.html\\'")
  :hook (web-mode . indent-guide-mode)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-engines-alist '(("ctemplate"    . "\\.hbs\\'"))))

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
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(use-package dired-sidebar
  :bind ("<f8>" . dired-sidebar-toggle-sidebar))


(use-package company
  :delight
  :hook (prog-mode . company-mode)
  :init
  (setq company-dabbrev-downcase nil))

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
  :hook (c-mode . irony-mode))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

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
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))


;;; Load private config
(load "~/.emacs.d/elisp.d/private.el")

;;; Various functions used in keybinds/hooks
(defun kill-dired-buffers ()
  "Kill all opened dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

;; Dealing with window scrolling
(defun window-half-height ()
  "Return 50% of the window height for scrolling by half."
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
  "Scroll up the page by 50%."
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-down-half ()
  "Scroll down the page by 50%."
  (interactive)
  (scroll-down (window-half-height)))

;; Cut & copy lines when cursor is over them
(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole
buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2017-12-04"
  (interactive)
  (if current-prefix-arg
      (progn
        (kill-ring-save (point-min) (point-max)))
    (if (use-region-p)
        (progn
          (kill-ring-save (region-beginning) (region-end)))
      (if (eq last-command this-command)
          (if (eobp)
              (progn )
            (progn
              (kill-append "\n" nil)
              (kill-append
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               nil)
              (progn
                (end-of-line)
                (forward-char))))
        (if (eobp)
            (if (eq (char-before) 10 )
                (progn )
              (progn
                (kill-ring-save (line-beginning-position) (line-end-position))
                (end-of-line)))
          (progn
            (kill-ring-save (line-beginning-position) (line-end-position))
            (end-of-line)
            (forward-char)))))))


(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole
buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(provide 'init)
;;; init.el ends here
