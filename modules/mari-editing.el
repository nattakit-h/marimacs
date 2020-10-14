;;; mari-editing.el --- Marimacs configuration file -*- lexical-binding: t -*-

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

(setq electric-pair-pairs '((?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")))
(electric-pair-mode t)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :hook
  (after-init . (lambda () (evil-mode 1))))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer nil)
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(use-package evil-magit
  :after evil magit)

(use-package yasnippet-snippets)

(use-package yasnippet
  :custom
  (yas-verbosity 0)
  :config
  (yas-global-mode))

(use-package cc-mode
  :straight nil
  :custom
  (c++-tab-always-indent t)
  (c-basic-offset 4)
  (c-indent-level 4)
  (tab-width 4)
  (indent-tabs-mode nil)
  :config
  (c-set-offset 'innamespace 0)
  (c-set-offset 'substatement-open 0))

(use-package editorconfig
  :hook
  (prog-mode . (lambda () (editorconfig-mode 1))))

(use-package mozc
  :straight nil)

(provide 'mari-editing)

;;; mari-editing.el ends here
