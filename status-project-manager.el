;;; status-project-manager.el

;; Copyright (C) 2014 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Keywords: emacs status project-manager

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

(defgroup status-project-manager nil
  "Status project-manager group."
  :group 'status)

(defcustom status-project-manager-fmt "%s"
  "Status format to display the current project-manager in the status area"
  :group 'status-project-manager)

(defface status-project-manager-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "RoyalBlue")))
  "face for current project-manager"
  :group 'status-project-manager)

(eval-after-load "project-manager"
  '(progn (defun status-project-manager ()
	    (let ((name (project-name current-project)))
	      (propertize (format status-project-manager-fmt name)
			'face 'status-project-manager-face)))))

(provide 'status-project-manager)
