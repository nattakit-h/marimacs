;;; mari-mail.el --- Marimacs configuration file -*- lexical-binding: t -*-

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

(defun mari:mail-headers ()
  "Get mu4e headers."
  (setq mu4e-headers-fields
        `((:date . 20)
          (:flags . 6)
          (:from . 27)
          (:thread-subject . ,(- (window-body-width) 67))
          (:size . 7))))

(defmacro mari:mail-match-func (mail)
  "Make match function for MAIL context."
  `(lambda (msg)
     (when msg
       (mu4e-message-contact-field-matches
        msg '(:from :to :cc :bcc) ,mail))))

(defun mari:mail-make-path (name path)
  "Make mail relative path string from NAME and PATH."
  (concat "/" (file-name-as-directory name) path))

(defun mari:mail-make-context (mail handle &optional fullname)
  "Make mail context from FULLNAME HANDLE and MAIL."
  (let ((archive-path (mari:mail-make-path handle "Archive"))
        (drafts-path (mari:mail-make-path handle "Drafts"))
        (sent-path (mari:mail-make-path handle "Sent"))
        (trash-path (mari:mail-make-path handle "Trash"))
        (inbox-path (mari:mail-make-path handle "Inbox")))
    (unless fullname (setq fullname handle))
    (make-mu4e-context
     :name handle
     :match-func (mari:mail-match-func mail)
     :vars `((user-mail-address . ,mail)
             (user-full-name . ,fullname)
             (mu4e-refile-folder . ,archive-path)
             (mu4e-drafts-folder . ,drafts-path)
             (mu4e-sent-folder . ,sent-path)
             (mu4e-trash-folder . ,trash-path)
             (mu4e-maildir-shortcuts . ((,inbox-path . ?i)
                                        (,sent-path . ?s)
                                        (,trash-path . ?t)
                                        (,archive-path . ?a)
                                        (,drafts-path . ?d)))))))

(use-package mu4e
  :straight nil
  :config
  (delete '("view as pdf" . mu4e-action-view-as-pdf) mu4e-view-actions)
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser) t)
  :custom
  (mu4e-get-mail-command "mbsync -a -c ~/.config/mbsync")
  (mu4e-maildir "~/.local/share/mail")
  (mu4e-view-show-images t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-index-update-error-warning t)
  (mu4e-index-update-error-continue t)
  (mu4e-hide-index-messages t)
  (mu4e-update-interval 300)
  (mu4e-index-lazy-check t)
  (mu4e-index-cleanup nil)
  (mu4e-view-show-addresses 't)
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'always-ask)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-headers-date-format "%Y-%m-%d %H:%M:%S")
  (message-kill-buffer-on-exit t)
  (mail-user-agent 'mu4e-user-agent)
  (browse-url-generic-program "chromium")
  (browse-url-browser-function 'browse-url-generic)
  :hook
  (m4u-headers-mode . mari:mail-headers))

(use-package mu4e-alert
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'libnotify)
  :hook
  (after-init . mu4e-alert-enable-notifications)
  (after-init . mu4e-alert-enable-mode-line-display))

(provide 'mari-mail)


;;; mari-mail.el ends here
