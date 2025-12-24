;;; org-techo.el --- Japanese-style planning for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jwalsh

;; Author: jwalsh <jwalsh@users.noreply.github.com>
;; Maintainer: jwalsh <jwalsh@users.noreply.github.com>
;; Created: 2025-12-24
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.5"))
;; Keywords: calendar, convenience, outlines
;; Homepage: https://github.com/aygp-dr/org-techo
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-techo brings the philosophy of Japanese paper planners
;; (like Hobonichi Techo) to Emacs org-mode.
;;
;; Features:
;; - Daily page generation with consistent structure
;; - Habit tracking
;; - Time blocking
;; - Weekly and monthly reviews

;;; Code:

(require 'org)
(require 'calendar)

(defgroup org-techo nil
  "Japanese-style planning for Org-mode."
  :group 'org
  :prefix "org-techo-")

(defcustom org-techo-directory "~/org/techo"
  "Directory where org-techo files are stored."
  :type 'directory
  :group 'org-techo)

(defcustom org-techo-daily-habits
  '("Morning pages" "Exercise" "Reading" "Meditation")
  "List of daily habits to track."
  :type '(repeat string)
  :group 'org-techo)

(defun org-techo-today ()
  "Return today's date as a string in YYYY-MM-DD format."
  (format-time-string "%Y-%m-%d"))

(defun org-techo-daily-file-name (&optional date)
  "Return the file name for DATE (defaults to today)."
  (let ((date-str (or date (org-techo-today))))
    (expand-file-name
     (format "%s/daily/%s.org" org-techo-directory date-str))))

(defun org-techo-create-daily-page (&optional date)
  "Create a daily page for DATE (defaults to today)."
  (interactive)
  (let* ((date-str (or date (org-techo-today)))
         (file-name (org-techo-daily-file-name date-str))
         (day-name (format-time-string "%A" (date-to-time date-str))))
    (find-file file-name)
    (when (= (buffer-size) 0)
      (insert (org-techo--daily-template date-str day-name))
      (save-buffer))
    (goto-char (point-min))
    (org-next-visible-heading 1)))

(defun org-techo--daily-template (date day-name)
  "Generate daily page template for DATE and DAY-NAME."
  (format "#+TITLE: %s - %s
#+DATE: %s
#+STARTUP: overview

* Daily Overview
** Intentions
- [ ]

** Time Blocks
| Time  | Activity |
|-------+----------|
| 06:00 |          |
| 07:00 |          |
| 08:00 |          |
| 09:00 |          |
| 10:00 |          |
| 11:00 |          |
| 12:00 |          |
| 13:00 |          |
| 14:00 |          |
| 15:00 |          |
| 16:00 |          |
| 17:00 |          |
| 18:00 |          |
| 19:00 |          |
| 20:00 |          |
| 21:00 |          |
| 22:00 |          |

* Habits
%s

* Notes

* Evening Reflection
** What went well?

** What could be improved?

** Gratitude

" date day-name date (org-techo--habits-checklist)))

(defun org-techo--habits-checklist ()
  "Generate habits checklist from `org-techo-daily-habits'."
  (mapconcat
   (lambda (habit) (format "- [ ] %s" habit))
   org-techo-daily-habits
   "\n"))

;;;###autoload
(defun org-techo-goto-today ()
  "Open or create today's daily page."
  (interactive)
  (org-techo-create-daily-page))

(provide 'org-techo)
;;; org-techo.el ends here
