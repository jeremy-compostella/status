;;; status-gnus.el

;; Copyright (C) 2014 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Keywords: emacs status gnus

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(defgroup status-gnus nil
  "Status gnus group."
  :group 'status)

(defcustom status-gnus-fmt "✉ %s"
  "Status format to display the current gnus in the status area"
  :group 'status-gnus)

(defcustom status-gnus-separator " "
  "GNUS status separator."
  :group 'status-gnus)

(defcustom status-gnus-groups '("INBOX")
  "GNUS group to get unread article number"
  :group 'status-gnus)

(defface status-gnus-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "yellow")))
  "face for current gnus"
  :group 'status-gnus)

(defun status-gnus-group-to-string (group)
  (let ((unread (gnus-group-unread group)))
    (unless (zerop unread)
      (format "%s:%d" group unread))))

(defun status-gnus-string ()
  (let ((groups (delq nil (mapcar 'status-gnus-group-to-string status-gnus-groups))))
    (when groups
      (format status-gnus-fmt
	      (mapconcat 'identity groups status-gnus-separator)))))

(eval-after-load "gnus-group"
  '(progn (defun status-gnus ()
	    (let ((status (status-gnus-string)))
	      (when status
		(propertize status 'face 'status-gnus-face))))))

(provide 'status-gnus)
