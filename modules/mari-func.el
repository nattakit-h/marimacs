;;; mari-func.el --- Marimacs configuration file -*- lexical-binding: t -*-

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mari:split-window-horizontally ()
  "Split window horizontally then shift focus to new windows."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun mari:split-window-vertically ()
  "Split window vertically then shift focus to new windows."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun mari:window-resize (direction &optional amount)
  "Resize current window toward up DIRECTION in AMOUNT distanst."
  (interactive)
  (unless amount (setq amount 5))
  (cond ((eq direction 'up)
         (windresize-up amount))
        ((eq direction 'down)
         (windresize-up (* -1 amount)))
        ((eq direction 'left)
         (windresize-left amount))
        ((eq direction 'right)
         (windresize-left (* -1 amount)))))

(defun mari:rename-current-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (new-name (read-file-name "New name: " (file-name-directory file-name)))
         (new-dir (file-name-directory new-name))
         (buffer-modified (buffer-modified-p)))
    (if (get-buffer new-name)
        (error "A buffer named '%s' already exists!" new-name))
    (set-visited-file-name new-name)
    (when (file-exists-p file-name)
      (unless (file-exists-p new-dir)
        (unless (yes-or-no-p (format "Create directory '%s'?" new-dir))
          (set-visited-file-name file-name)
          (set-buffer-modified-p buffer-modified)
          (error "No such directory"))
        (make-directory new-dir t))
      (rename-file file-name new-name t)
      (set-visited-file-name new-name)
      (set-buffer-modified-p buffer-modified)
      (message "File '%s' successfully renamed to '%s'" file-name new-name))))

(defun mari:delete-current-file ()
  "Delete current file and buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer)
        (when (yes-or-no-p "Are you sure you want to delete this file? ")
          (delete-file filename t)
          (kill-buffer buffer)))))

(defun mari:eql-start-swank ()
  "Start swank with eql5."
  (interactive)
  (unless (get-process "eql-swank")
    (start-process "eql-swank" "*eql-swank*" "eql5"
                   (concat user-emacs-directory
                           "straight/repos/slime/eql-start-swank.lisp"))))

(defun mari:ecl-slime ()
  "Start and connect to swank with ecl."
  (interactive)
  (setq inferior-lisp-program "ecl")
  (slime)
  (run-at-time 0.1 nil (lambda () (shrink-window 15) (other-window 1))))

(defun mari:eql-slime ()
  "Start and connect to swank with eql5."
  (interactive)
  (setq inferior-lisp-program "eql5")
  (unless (get-buffer "*slime-repl ECL*")
    (mari:eql-start-swank)
    (with-timeout (1 (message "Unable to connect to eql-swank process"))
      (while (not (slime-connected-p))
        (ignore-errors (slime-connect "localhost" 4005 nil nil))))
    ;; HACK: Waiting for pop up window
    (run-at-time 0.1 nil (lambda () (shrink-window 15) (other-window 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mari:with-fa-icon (icon str &optional height v-adjust)
  "Get font awesome icon named ICON with specified HEIGHT and V-ADJUST \
and append it wtih STR."
  (s-concat (all-the-icons-faicon
             icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun mari:project-root ()
  "Get current project root."
  (project-root (project-current)))

(defun mari:project-name ()
  "Get current project name."
  (file-name-nondirectory (directory-file-name (mari:project-root))))

(defun mari:project-file-relative-path ()
  "Get current file path relative to project root."
  (let ((root (expand-file-name (mari:project-root)))
        (path (file-name-directory (buffer-file-name))))
    (string-match root path)
    (replace-match "" nil nil path)))

(defun mari:string-c-header-guard ()
  "Get string for c/c++ header guard macros."
  (let ((project (mari:project-name))
        (path (mari:project-file-relative-path))
        (name (file-name-base (buffer-file-name)))
        (ext (file-name-extension (buffer-file-name) nil)))
    (upcase (replace-regexp-in-string
             "[^a-zA-Z0-9]" "_"
             (concat project "_" path name "_" ext)))))

(defun mari:racket-repl-clear ()
  "Clear Racket REPL if it exist."
  (pcase (get-buffer "*Racket REPL*")
    (bufffer (when bufffer (with-current-buffer bufffer
                             (comint-kill-region (point-min) (point-max)))))))

(defun mari:get-xdg-data-home ()
  "Get user XDG data home directory."
  (file-name-as-directory
   (or (getenv "XDG_DATA_HOME")
       (expand-file-name "share" (expand-file-name ".local" (getenv "HOME"))))))

(provide 'mari-func)

;;; mari-func.el ends here
