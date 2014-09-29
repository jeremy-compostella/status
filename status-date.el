;;; status-date.el

;; Copyright (C) 2014 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Keywords: emacs status date

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

(defgroup status-date nil
  "Status date and time group."
  :group 'status)

(defcustom status-date-fmt "%d/%m/%Y "
  "Date format used to display current date in the status area")

(defcustom status-time-fmt "%Hh%M"
  "Date format used to display current time in the status area")

(defface status-date-face
  '((t (:width ultra-expanded :inherit variable-pitch
	       :foreground "RoyalBlue")))
  "face for date and time"
  :group 'status-date)

(defface status-time-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "RoyalBlue")))
  "face for date and time"
  :group 'status-date)

(defun status-date ()
  "Issue the date and time string update and should be called via a timer"
  (concat (propertize (format-time-string status-date-fmt (current-time)) 'face 'status-date-face)
	  (propertize (format-time-string status-time-fmt (current-time)) 'face 'status-time-face)))

(provide 'status-date)
