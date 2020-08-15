;;; early-init.el --- Early init file                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Austin Norberg

;; Author: Austin Norberg <austin@norberg.tech>

;;; Commentary:

;;

;;; Code:
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq site-run-file nil)

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)

(provide 'early-init)
;;; early-init.el ends here
