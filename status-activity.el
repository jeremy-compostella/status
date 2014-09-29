;;; status-activity.el

;; Copyright (C) 2014 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Keywords: emacs status activity

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

(defgroup status-activity nil
  "Status activity group."
  :group 'status)

(defcustom status-activity-fmt "%s"
  "Status format to display the current activity in the status area")

(defface status-activity-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "orange")))
  "face for current activity"
  :group 'status-activity)

(eval-after-load "activity"
  '(progn (defun status-activity ()
	    (propertize (format status-activity-fmt
				(activity-name (current-activity)))
			'face 'status-activity-face))
	  (add-hook 'toggle-activity-hooks 'status-update)))

(provide 'status-activity)
