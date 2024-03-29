;;; -*- lexical-binding: t; -*-

(defun an/kill-dired-buffers ()
  "Kill all opened dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

;; Dealing with window scrolling
(defun an/window-half-height ()
  "Return 50% of the window height for scrolling by half."
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun an/scroll-up-half ()
  "Scroll up the page by 50%."
  (interactive)
  (scroll-up (an/window-half-height)))

(defun an/scroll-down-half ()
  "Scroll down the page by 50%."
  (interactive)
  (scroll-down (an/window-half-height)))

;; Cut & copy lines when cursor is over them
(defun an/xah-copy-line-or-region ()
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


(defun an/xah-cut-line-or-region ()
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

;; From http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun an/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun an/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (an/delete-word (- arg)))

;; Modified version of...
;; http://jacek.zlydach.pl/blog/2018-05-29-serving-directories-over-http-with-emacs.html
(defvar an/file-server nil "Is the file server running? Holds an instance if so.")

(defun an/serve-directory (directory port)
  "Serve DIRECTORY over http via PORT."
  (interactive "DDirectory: \nnPort: ")
  (if an/file-server
      (message "File server is already running.")
    (progn
      (setq an/file-server
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

(defun an/stop-server ()
  "Stop the file server if running."
  (interactive)
  (if an/file-server
      (progn
        (ws-stop an/file-server)
        (setf an/file-server nil)
        (message "Stopped the file server."))
    (message "No file server is running.")))

(defun an/split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun an/split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun an/indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defvar an/current-theme 'modus-operandi)

(defun an/toggle-dark-theme ()
  "Toggle between dark/light Emacs and GTK themes."
  (interactive)
  (let ((emacs-theme (if (eq an/current-theme 'modus-operandi) 'modus-vivendi 'modus-operandi))
        (gtk-theme (if (eq an/current-theme 'modus-operandi) "'Adwaita-dark'" "'Adwaita'")))
    (load-theme emacs-theme)
    (start-process-shell-command "gsettings" nil (concat "gsettings set org.gnome.desktop.interface gtk-theme " gtk-theme))
    (setq an/current-theme emacs-theme)))

(defun an/generate-new-password (length symbols)
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

(defun an/elfeed-enclosure-yank ()
  "Grab the enclosure URL and return it."
  (let* ((entry (elfeed-search-selected t))
         (url (replace-regexp-in-string "\\?.*$" "" (caar (elfeed-entry-enclosures entry)))))
    url))

(defun an/elfeed-link-yank ()
  "Grab the enclosure URL and return it."
  (let* ((entry (elfeed-search-selected t))
         (url (elfeed-entry-link entry)))
    url))

(defun an/elfeed-download-media ()
  "Download youtube videos and podcasts."
  (interactive)
  (let* ((entry (elfeed-search-selected t))
         (tags (elfeed-entry-tags entry))
         (url (cond ((member 'podcast tags) (an/elfeed-enclosure-yank))
                    ((member 'video tags) (an/elfeed-link-yank)))))
    (message "Downloading media from: %s" url)
    (start-process "elfeed-youtube-dl" nil "youtube-dl" url "-o" "~/Downloads/%(title)s.%(ext)s")))

;; these elfeed functions courtesy of https://github.com/skeeto/elfeed/issues/267
(defun an/elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (an/open-in-mpv (elfeed-entry-link entry))))

(defun an/elfeed-open-with-eww ()
  "Open in eww with `eww-readable'."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (eww  (elfeed-entry-link entry))
    (add-hook 'eww-after-render-hook 'eww-readable nil t)))

(defun an/elfeed-visit-maybe-externally ()
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

(defun an/change-desktop ()
  "Interface to easily switch desktops."
  (interactive)
  (let* ((desktops (remove "." (remove ".." (directory-files "~/.config/emacs/desktops"))))
         (selection (completing-read "Change desktop: " desktops)))
    (desktop-read (format "%s%s" "~/.config/emacs/desktops/" selection))))

(defun an/open-in-mpv (link)
  "Open LINK in mpv."
  (start-process "mpv" nil "celluloid" link))

;; org link yanking functions are modified versions of https://emacs.stackexchange.com/questions/3981/how-to-copy-links-out-of-org-mode
(defun an/yank-org-link (text)
  "Yank TEXT from org-link string."
  (string-match org-bracket-link-regexp text)
  (insert (substring text (match-beginning 1) (match-end 1))))

(defun an/get-org-link-substring ()
  "Get substring of org-link."
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                               (or (caddr link-info) (point-max))))))
    text))

(defun an/org-copy-link-at-point ()
  "Copy org-link at point."
  (interactive)
  (let* ((text (an/get-org-link-substring)))
    (if (not text)
        (error "Not in org link")
      (add-text-properties 0 (length text) '(yank-handler (an/yank-org-link)) text)
      (kill-new text)
      (string-match org-bracket-link-regexp text)
      (funcall interprogram-cut-function (substring text (match-beginning 1) (match-end 1))))))

(defun an/org-link-open-in-mpv ()
  "Open org-link at point in mpv."
  (interactive)
  (let* ((text (an/get-org-link-substring)))
    (if (not text)
        (error "Not in org link")
      (string-match org-bracket-link-regexp text)
      (print (substring text (match-beginning 1) (match-end 1)))
      (an/open-in-mpv (substring text (match-beginning 1) (match-end 1))))))

(defun an/org-mark-done ()
  (interactive)
  (org-agenda-todo "DONE"))

(defun an/replace-angled-quotes ()
  "Replace all angled single & double quotes with straight variant in buffer."
  (interactive)
  (goto-char 1)
  (while (re-search-forward "”" nil t)
    (replace-match "\""))
  (goto-char 1)
  (while (re-search-forward "“" nil t)
    (replace-match "\""))
  (goto-char 1)
  (while (re-search-forward "’" nil t)
    (replace-match "'"))
  (goto-char 1)
  (while (re-search-forward "‘" nil t)
    (replace-match "'")))

; Courtesy of https://christiantietze.de/posts/2022/04/emacs-center-window-current-monitor-simplified/
(defun an/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.5) (left . 0.5)))))

;; Org clock mode line functionality from...
;; http://mbork.pl/2019-05-11_Toggling_modeline_clock_display
(setq org-clock-mode-line-total-settings
      '((current . "time spent in this chunk on the current task")
	(today . "time spent today on the current task")
	(all . "total time spent on the current task")))
(setq org-clock-mode-line-total-setting-number 0)

(defun toggle-org-clock-mode-line-total-setting (setting-number)
  "Toggle between org-clock-mode-line-total settings.
With a numeric argument, use setting SETTING-NUMBER."
  (interactive "P")
  (if (numberp setting-number)
      (setq org-clock-mode-line-total-setting-number
	    (mod setting-number (length org-clock-mode-line-total-settings)))
    (setq org-clock-mode-line-total-setting-number
	  (mod (1+ org-clock-mode-line-total-setting-number)
	       (length org-clock-mode-line-total-settings))))
  (let ((org-clock-mode-line-total-setting (nth org-clock-mode-line-total-setting-number
						org-clock-mode-line-total-settings)))
    (setq org-clock-mode-line-total (car org-clock-mode-line-total-setting))
    (when (org-clocking-p)
      (setq org-clock-total-time
	    (with-current-buffer (marker-buffer org-clock-hd-marker)
	      (save-excursion (goto-char org-clock-hd-marker)
			      (org-clock-sum-current-item
			       (org-clock-get-sum-start)))))
      (org-clock-update-mode-line))
    (message "Modeline shows %s."
	     (cdr org-clock-mode-line-total-setting))))

(provide 'misc-defuns)

;;; misc-defuns.el ends here
