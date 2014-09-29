;;; status-battery.el

;; Copyright (C) 2014 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Keywords: emacs status battery

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

(require 'battery)

(defgroup status-battery nil
  "Status battery group."
  :group 'status)

(defcustom status-battery-fmt "%s"
  "Status format to display the current battery in the status area"
  :group 'status-battery)

(defvar status-battery-charging-fmt "%p%% %t")

(defface status-battery-charging-face
  '((t (:width ultra-expanded :inherit variable-pitch :foreground "RoyalBlue")))
  "face for date and time"
  :group 'status-faces)

(defvar status-battery-charged-fmt "%p%%")

(defface status-battery-charged-face
  '((t (:width ultra-expanded :inherit variable-pitch :foreground "green")))
  "face for date and time"
  :group 'status-faces)

(defvar status-battery-discharging-fmt "%p%% %rW %t")

(defface status-battery-discharging-face
  '((t (:width ultra-expanded :inherit variable-pitch :foreground "brown1")))
  "face for date and time"
  :group 'status-faces)

(defun status-battery ()
  (let ((batt-data (funcall battery-status-function)))
    (flet ((make-batt-string (fmt face) (propertize (battery-format fmt batt-data) 'face face)))
      (cond ((string= "Discharging" (cdr (assq ?B batt-data)))
	     (make-batt-string status-battery-discharging-fmt 'status-battery-discharging-face))
	    ((string= "100.0" (cdr (assq ?p batt-data)))
	     (make-batt-string status-battery-charged-fmt 'status-battery-charged-face))
	    (t (make-batt-string status-battery-charging-fmt 'status-battery-charging-face))))))

(provide 'status-battery)
