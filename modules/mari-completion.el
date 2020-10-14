;;; mari-completion.el --- Marimacs configuration file -*- lexical-binding: t -*-

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

(use-package selectrum
  :config
  (selectrum-mode))

(use-package company
  :custom
  (company-idle-delay 0)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  :hook
  (prog-mode . company-mode)
  ((c-mode c++-mode) . (lambda () (setq company-backends (delete 'company-clang company-backends)))))

(use-package company-box
  :after company all-the-icons
  :custom
  (company-box-show-single-candidate t)
  (company-box-max-candidates 50)
  (company-box-doc-delay 0.5)
  (company-box-icons-all-the-icons
   `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
     (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
     (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
     (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
     (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
     (Field . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
     (Variable . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
     (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
     (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
     (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
     (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
     (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
     (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
     (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
     (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
     (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
     (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
     (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
     (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
     (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
     (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
     (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
     (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
     (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
     (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
     (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
     (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))))
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  :hook
  (company-mode . company-box-mode))

(provide 'mari-completion)

;;; mari-completion.el ends here
