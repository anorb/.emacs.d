;;; -*- lexical-binding: t; -*-

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



;; Modified version of...
;; http://jacek.zlydach.pl/blog/2018-05-29-serving-directories-over-http-with-emacs.html
(defvar my/file-server nil "Is the file server running? Holds an instance if so.")

(defun my/serve-directory (directory port)
  "Serve DIRECTORY over http via PORT."
  (interactive "DDirectory: \nnPort: ")
  (if my/file-server
      (message "File server is already running.")
    (progn
      (setq my/file-server
            (let ((docroot directory))
              (ws-start
               (lambda (request)
                 (with-slots (process headers) request
                   (let* ((path (substring (cdr (assoc :GET headers)) 1))
                          (expanded (ws-in-directory-p docroot path)))
                     (if (and expanded
                              (file-exists-p expanded))
                         (if (file-directory-p expanded)
                             (ws-send-directory-list process expanded)
                           (ws-send-file process (expand-file-name path docroot)))
                       (ws-send-404 process)))))
               port
               nil
               :host "0.0.0.0")))
      (message "Serving directory %s on port %d" directory port))))

(defun my/stop-server ()
  "Stop the file server if running."
  (interactive)
  (if my/file-server
      (progn
        (ws-stop my/file-server)
        (setf my/file-server nil)
        (message "Stopped the file server."))
    (message "No file server is running.")))

(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun generate-new-password (length symbols)
  "Generate a password with pwgen with LENGTH characters.

If SYMBOLS is t, symbols will be added to the password."
  (interactive
   (list (read-number "Length: ")
         (y-or-n-p "Add symbols? ")))
  (let* ((pwgen-command (if symbols
                            (concat "pwgen -s -1 -y " (number-to-string length))
                          (concat "pwgen -s -1 " (number-to-string length))))
         (generated-password (s-trim (shell-command-to-string pwgen-command))))
    (insert generated-password)
    (funcall interprogram-cut-function generated-password)))

(defun elfeed-enclosure-yank ()
  "Grab the enclosure URL and return it."
  (let* ((entry (elfeed-search-selected t))
         (url (replace-regexp-in-string "\\?.*$" "" (caar (elfeed-entry-enclosures entry)))))
    url))

(defun elfeed-link-yank ()
  "Grab the enclosure URL and return it."
  (let* ((entry (elfeed-search-selected t))
         (url (elfeed-entry-link entry)))
    url))

(defun elfeed-download-media ()
  "Download youtube videos and podcasts."
  (interactive)
  (let* ((entry (elfeed-search-selected t))
         (tags (elfeed-entry-tags entry))
         (url (cond ((member 'podcast tags) (elfeed-enclosure-yank))
                    ((member 'youtube tags) (elfeed-link-yank)))))
    (message "Downloading media from: %s" url)
    (start-process "elfeed-youtube-dl" nil "youtube-dl" url "-o" "~/Downloads/%(title)s.%(ext)s")))

;; these elfeed functions courtesy of https://github.com/skeeto/elfeed/issues/267
(defun elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))

(defun elfeed-open-with-eww ()
  "Open in eww with `eww-readable'."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (eww  (elfeed-entry-link entry))
    (add-hook 'eww-after-render-hook 'eww-readable nil t)))

(defun elfeed-visit-maybe-externally ()
  "Visit with external function if entry link matches `elfeed-visit-patterns',
show normally otherwise."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
                   elfeed-show-entry
                 (elfeed-search-selected :single)))
        (patterns elfeed-visit-patterns))
    (while (and patterns (not (string-match (caar patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (cond
     (patterns
      (funcall (cdar patterns)))
     ((eq major-mode 'elfeed-search-mode)
      (call-interactively 'elfeed-search-show-entry))
     (t (elfeed-show-visit)))))

(defun an/change-desktop()
  "Interface to easily switch desktops."
  (interactive)
  (let* ((desktops (remove "." (remove ".." (directory-files "~/.emacs.d/desktops"))))
         (selection (completing-read "Change desktop:" desktops)))
    (desktop-read (format "%s%s" "~/.emacs.d/desktops/" selection))))

(provide 'misc-defuns)

;;; misc-defuns.el ends here
