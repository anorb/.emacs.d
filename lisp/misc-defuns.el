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
               nil)))
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

(defun delete-window-balance ()
  "Delete window and rebalance the remaining ones."
  (interactive)
  (delete-window)
  (balance-windows))

(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (balance-windows)
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
