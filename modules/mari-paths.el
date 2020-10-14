;;; mari-paths.el --- Marimacs configuration file -*- lexical-binding: t -*-

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

(use-package no-littering
  :custom
  (no-littering-etc-directory mari:data-directory)
  (no-littering-var-directory mari:runtime-directory)
  (custom-file (expand-file-name "custom.el" mari:data-directory))
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save" mari:runtime-directory) t))))

(provide 'mari-paths)

;;; mari-paths.el ends here
