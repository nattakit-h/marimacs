;;; mari-lang.el --- Marimacs configuration file -*- lexical-binding: t -*-

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

(use-package clang-format+
  :custom
  (clang-format+-always-enable t)
  :hook ((c-mode c++-mode) . clang-format+-mode))

(use-package tsc
  :straight (tsc
             :type git
             :host github
             :repo "ubolonton/emacs-tree-sitter"
             :files ("core/*.el")))

(use-package tree-sitter
  :after tsc
  :straight (tree-sitter
             :type git
             :host github
             :repo "ubolonton/emacs-tree-sitter"
             :files ("lisp/*.el"))
  :hook
  ((c-mode c++mode) . tree-sitter-mode)
  ((c-mode c++-mode) . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter
  :straight (tree-sitter-langs
             :type git
             :host github
             :repo "ubolonton/emacs-tree-sitter"
             :files ("langs/*.el" "langs/queries")))

(use-package ccls
  :custom
  (ccls-sem-highlight-method 'font-lock)
  (ccls-initialization-options
   '(:index (:comments 2 :onChange t) :completion
            (:detailedLabel t) :cache (:directory ""))))

(use-package arduino-mode)

(use-package json-mode)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '(".clang-format\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '(".clang-tidy\\'" . yaml-mode)))

(provide 'mari-lang)

;;; mari-lang.el ends here
