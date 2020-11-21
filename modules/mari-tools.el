;;; mari-tools.el --- Marimacs configuration file -*- lexical-binding: t -*-

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

(use-package magit
  :custom
  (magit-auto-revert-mode nil))

(use-package shell-pop
  :custom
  (shell-pop-shell-type (quote ("term" "*term*" (lambda nil (term shell-pop-term-shell)))))
  (shell-pop-window-position "buttom")
  (shell-pop-window-size 20))

(use-package elfeed)

(use-package w3m
  :custom
  (w3m-use-cookie nil))

(use-package ctrlf
  :config
  (ctrlf-mode 1))

(provide 'mari-tools)

;;; mari-tools.el ends here
