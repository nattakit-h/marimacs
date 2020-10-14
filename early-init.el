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
  (concat user-emacs-directory "modules"))

(defconst mari:runtime-directory
  (concat user-emacs-directory "runtime"))

(defconst mari:data-directory
  (concat user-emacs-directory "data"))

;; Increase gc threshold for lsp-mode
(setq gc-cons-threshold 100000000)

;; Disabled auto save
(setq auto-save-default nil)

;; Disabled backup file
(setq make-backup-files nil)

;; Quiet startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(defalias 'display-startup-echo-area-message 'ignore)

;; Set titlebar
(setq frame-title-format "marimacs")

;; Do not resize the frame
(setq frame-inhibit-implied-resize t)

;; Disable ring-bell
(setq ring-bell-function 'ignore)

;; Disable line wrapping
(add-hook 'hack-local-variables-hook
          (lambda () (setq truncate-lines t)))

;; Turn yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable most GUI elements before they're loaded
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Add load path
(add-to-list 'load-path mari:modules-directory)

;;; early-init.el ends here
