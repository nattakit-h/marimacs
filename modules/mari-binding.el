;;; mari-binding.el --- Marimacs configuration file -*- lexical-binding: t -*-

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

(use-package avy)

(use-package ace-window
  :custom
  (aw-ignore-current t)
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(defmacro mari:defkey (key func &optional map)
  "Bind KEY to FUNC in MAP."
  (let ((keymap (or map 'evil-normal-state-map)))
    `(define-key ,keymap (kbd ,key) ,func)))

(defmacro mari:com (args &rest body)
  "Alias for interactive command with ARGS and BODY."
  `(lambda ,args
     (interactive)
     ,@body))

(mari:defkey "C-c f r" #'mari:rename-current-file)
(mari:defkey "C-c f d" #'mari:delete-current-file)
(mari:defkey "C-c t t" #'shell-pop)
(mari:defkey "C-x 2" (mari:com () (split-window-below) (other-window 1)))
(mari:defkey "C-x 3" (mari:com () (split-window-right) (other-window 1)))
(mari:defkey "C-j" #'next-line selectrum-minibuffer-map)
(mari:defkey "C-k" #'previous-line selectrum-minibuffer-map)
(mari:defkey "C-;" #'avy-goto-char-timer)
(mari:defkey "C-." #'avy-goto-word-1)
(mari:defkey "C-x o" #'ace-window global-map)

(provide 'mari-binding)

;;; mari-binding.el ends here
