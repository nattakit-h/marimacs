;;; mari-lsp.el --- Marimacs configuration file -*- lexical-binding: t -*-

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

(use-package flycheck
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc emacs-lisp))
  :hook
  (prog-mode . flycheck-mode))

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-enable-snippet t)
  (lsp-enable-semantic-highlighting t)
  (lsp-file-watch-threshold nil)
  (lsp-completion-provider :capf)
  (read-process-output-max (* 1024 1024))
  :hook
  ;; pip install cmake-language-server
  ((cmake-mode) . lsp)
  ;; ccls
  ((c-mode c++-mode) . lsp))

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :commands lsp-ui-mode)

(provide 'mari-lsp)

;;; mari-lsp.el ends here
