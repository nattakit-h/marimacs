;;; mari-appearance.el --- Marimacs configuration file -*- lexical-binding: t -*-

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

;; Relative line numbers
(use-package display-line-numbers
  :straight nil
  :custom
  (display-line-numbers-type 'relative)
  :config
  (global-display-line-numbers-mode))

;; Icons
(use-package all-the-icons)

;; Icons for dired
(use-package all-the-icons-dired
  :after all-the-icons
  :hook
  (dired-mode . all-the-icons-dired-mode))


;; Display trailing whitespace
(use-package whitespace
  :straight nil
  :custom
  (whitespace-line-column 120)
  (whitespace-style '(face tabs trailing line-trail tab-mark))
  (whitespace-display-mappings '((space-mark 32 [183] [46])
                                 (newline-mark 10 [8991 10])
                                 (tab-mark 9 [9655 9] [92 9])))
  :hook (prog-mode . whitespace-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :after all-the-icons
  :config
  (show-paren-mode 1)
  (set-face-foreground 'rainbow-delimiters-depth-2-face
                       (face-foreground 'all-the-icons-blue))
  (set-face-foreground 'rainbow-delimiters-depth-3-face
                       (face-foreground 'all-the-icons-lgreen))
  (set-face-foreground 'rainbow-delimiters-depth-4-face
                       (face-foreground 'all-the-icons-yellow))
  (set-face-foreground 'rainbow-delimiters-depth-5-face
                       (face-foreground 'all-the-icons-red))
  :hook
  ((emacs-lisp-mode racket-mode) . rainbow-delimiters-mode))

;; Doom themes
(use-package doom-themes
  :config
  (load-theme 'doom-moonlight t)
  (set-face-foreground 'link "#C3E88D"))

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Highlight special comments
(use-package hl-todo
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO" warning bold)
          ("FIXME" error bold)
          ("HACK" font-lock-constant-face bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("NOTE" success bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("BUG" error bold)))
  :hook
  (after-init . global-hl-todo-mode))

;; Dashboard
(use-package dashboard
  :after all-the-icons
  :custom
  (dashboard-startup-banner (concat (file-name-directory user-emacs-directory) "emacs.png"))
  (dashboard-banner-logo-title "M A R I M A C S")
  (dashboard-items '())
  (dashboard-set-navigator t)
  (dashboard-show-shortcuts nil)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-set-footer nil)
  :config
  (dashboard-setup-startup-hook))

(provide 'mari-appearance)

;;; mari-appearance.el ends here
