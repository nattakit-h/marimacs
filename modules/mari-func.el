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

;; Interactive functions

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
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (save-buffer)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun mari:delete-current-file ()
  "Delete current file and buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
        (when (yes-or-no-p "Are you sure you want to delete this file? ")
          (delete-file filename t)
          (kill-buffer buffer)))))

;; Non-interactive functions

(defun mari:with-fa-icon (icon str &optional height v-adjust)
  "Get font awesome icon named ICON with specified HEIGHT and V-ADJUST and append it wtih STR."
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

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
        (ext (file-name-extension (buffer-file-name) nil))
        (result ""))
    (setq result (upcase (replace-regexp-in-string "[^a-zA-Z0-9]" "_" (concat project "_" path name "_" ext))))
    result))

(defun mari:racket-repl-clear ()
  "Clear Racket REPL if it exist."
  (pcase (get-buffer "*Racket REPL*")
    (bufffer (when bufffer (with-current-buffer bufffer
                             (comint-kill-region (point-min) (point-max)))))))


(provide 'mari-func)

;;; mari-func.el ends here
