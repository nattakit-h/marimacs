;;; early-init.el --- Marimacs configuration file -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nattakit Hosapin

;; Author: Nattakit Hosapin <nattakit@hosapin.com>
;; URL: https://github.com/nattakit-h/marimacs

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; Set standard paths
(defconst mari:modules-directory
  (expand-file-name "modules" user-emacs-directory))

(defconst mari:runtime-directory
  (expand-file-name "runtime" user-emacs-directory))

(defconst mari:data-directory
  (expand-file-name "data" user-emacs-directory))

;; Set eln cache directory
(when (featurep 'nativecomp)
  (setq comp-eln-load-path
        (cons (expand-file-name "eln" mari:runtime-directory)
              (remq (car comp-eln-load-path) comp-eln-load-path))))

;; Disabled auto save
(setq auto-save-default nil)

;; Disabled backup file
(setq make-backup-files nil)

;; Quiet startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(defalias 'display-startup-echo-area-message 'ignore)

;; Disable dialog box
(setq use-dialog-box nil)

;; Disable native tooltips
(setq x-gtk-use-system-tooltips nil)

;; Set titlebar
(setq frame-title-format "marimacs")

;; Do not resize the frame
(setq frame-inhibit-implied-resize t)

;; Disable ring-bell
(setq ring-bell-function 'ignore)

;; Disable line wrapping
(add-hook 'hack-local-variables-hook
          (lambda () (setq truncate-lines t)))

;; Don't save custom file
(setq custom-file null-device)

;; Turn yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable most GUI elements before they're loaded
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Explicitly set text encoding to UTF-8.
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Add load path
(add-to-list 'load-path mari:modules-directory)

;; Set default browser
(setq w3m-use-tab nil)
(setq browse-url-handlers '(("." . w3m-browse-url)))

;; Kill all remaining processes when emacs exited
(setq confirm-kill-processes nil)

;; Enable so long mode
(global-so-long-mode 1)

;; Disable downcase-region
(put 'downcase-region 'disabled nil)

;;; early-init.el ends here
