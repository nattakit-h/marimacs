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

(use-package ace-window)

(use-package windresize
  :config
  (define-key evil-normal-state-map (kbd "C-x r") 'mari:menu-window-resize/body))

(use-package hydra
  :config
  (defhydra mari:menu-index (:color blue)
    "index"
    ("f" mari:menu-file/body "file")
    ("w" mari:menu-window/body "window")
    ("p" mari:menu-project/body "project")
    ("t" mari:menu-tools/body "tools"))

  (defhydra mari:menu-file (:color blue)
    "file"
    ("f" find-file "find")
    ("s" save-buffer "save")
    ("r" mari:rename-current-file "rename")
    ("d" mari:delete-current-file "delete"))

  (defhydra mari:menu-window (:color blue)
    "window"
    ("w" ace-window "jump")
    ("l" mari:split-window-horizontally "new h")
    ("j" mari:split-window-vertically "new v")
    ("r" mari:menu-window-resize/body "resize")
    ("d" delete-window "delete"))

  (defhydra mari:menu-window-resize (:color amaranth)
    "window resize"
    ("k" (mari:window-resize 'up) "Up")
    ("j" (mari:window-resize 'down) "Down")
    ("h" (mari:window-resize 'left) "Left")
    ("l" (mari:window-resize 'right) "Right")
    ("q" nil "quit"))

  (defhydra mari:menu-project (:color blue)
    "project"
    ("f" project-find-file "find file")
    ("p" project-switch-project "switch")
    ("g" project-find-regexp "search")
    ("k" project-kill-buffers "close"))

  (defhydra mari:menu-tools (:color blue)
    "tools"
    ("c" execute-extended-command "command")
    ("t" shell-pop "term")
    ("g" magit "git")
    ("d" (dired nil) "dired")
    ("m" mu4e "mail"))

  (global-set-key (kbd "<muhenkan>") 'mari:menu-index/body)
  (global-set-key (kbd "<f1>") 'mari:menu-index/body))

(provide 'mari-binding)

;;; mari-binding.el ends here
