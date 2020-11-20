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

(use-package ccls
  :custom
  (ccls-sem-highlight-method 'overlay)
  (ccls-initialization-options
   '(:index (:comments 2 :onChange t) :completion
            (:detailedLabel t) :cache (:directory ""))))

(use-package json-mode)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '(".clang-format\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '(".clang-tidy\\'" . yaml-mode)))

(use-package org
  :straight nil
  :custom
  (org-hide-emphasis-markers t)
  (org-fontify-done-headline t)
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  (org-odd-levels-only t)
  (org-startup-indented t)
  (org-src-tab-acts-natively t)
  (prettify-symbols-unprettify-at-point 'right-edge)
  (prettify-symbols-alist (append
                           '(("#+BEGIN_SRC" . "⇀")
                             ("#+END_SRC" . "↽"))
                           prettify-symbols-alist))
  :hook
  (org-mode . prettify-symbols-mode))

(use-package org-bullets
  :custom
  (org-ellipsis "*")
  :hook
  (org-mode . org-bullets-mode))

(use-package geiser
  :custom
  (geiser-active-implementations '(racket)))

(use-package slime
  :straight (slime :type git :host github :repo "nattakit-h/slime")
  :custom
  (inferior-lisp-program "ecl")
  (slime-contribs '(slime-fancy slime-company))
  (slime-repl-history-file (concat mari:runtime-directory "slime-history.eld"))
  :config
  (load (concat (mari:get-xdg-data-home) "quicklisp/clhs-use-local.el") t t)
  :hook
  (lisp-mode . mari:eql-slime))

(use-package slime-company
  :after (slime company)
  :custom
  (slime-company-completion 'fuzzy))

(slime-setup)

(provide 'mari-lang)

;;; mari-lang.el ends here
