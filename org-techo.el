;;; org-techo.el --- Japanese-style planning for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jwalsh

;; Author: jwalsh <jwalsh@users.noreply.github.com>
;; Maintainer: jwalsh <jwalsh@users.noreply.github.com>
;; Created: 2025-12-24
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (org "9.5") (transient "0.4.0"))
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
(require 'transient)

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

;;; ----------------------------------------------------------------------------
;;; Date Navigation
;;; ----------------------------------------------------------------------------

(defun org-techo--date-offset (days &optional from-date)
  "Return date string DAYS from FROM-DATE (default today)."
  (let* ((base (if from-date
                   (date-to-time from-date)
                 (current-time)))
         (offset (days-to-time days))
         (new-time (time-add base offset)))
    (format-time-string "%Y-%m-%d" new-time)))

;;;###autoload
(defun org-techo-goto-today ()
  "Open or create today's daily page."
  (interactive)
  (org-techo-create-daily-page))

;;;###autoload
(defun org-techo-goto-yesterday ()
  "Open or create yesterday's daily page."
  (interactive)
  (org-techo-create-daily-page (org-techo--date-offset -1)))

;;;###autoload
(defun org-techo-goto-tomorrow ()
  "Open or create tomorrow's daily page."
  (interactive)
  (org-techo-create-daily-page (org-techo--date-offset 1)))

;;;###autoload
(defun org-techo-goto-date ()
  "Open or create daily page for a specific date."
  (interactive)
  (let ((date (org-read-date nil nil nil "Go to date: ")))
    (org-techo-create-daily-page date)))

;;;###autoload
(defun org-techo-list-days ()
  "List all daily pages in a buffer."
  (interactive)
  (let ((dir (expand-file-name "daily" org-techo-directory)))
    (if (file-directory-p dir)
        (dired dir)
      (message "No daily pages yet. Create one with org-techo-goto-today."))))

;;; ----------------------------------------------------------------------------
;;; Habit Commands
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun org-techo-toggle-habit ()
  "Toggle the habit checkbox on the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "\\[[ X]\\]" (line-end-position) t)
      (backward-char 2)
      (if (looking-at "X")
          (progn (delete-char 1) (insert " "))
        (delete-char 1) (insert "X")))))

;;;###autoload
(defun org-techo-habits-summary ()
  "Show summary of today's habits."
  (interactive)
  (let* ((file (org-techo-daily-file-name))
         (completed 0)
         (total 0))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "^- \\[\\([ X]\\)\\]" nil t)
            (setq total (1+ total))
            (when (string= (match-string 1) "X")
              (setq completed (1+ completed))))
          (message "Habits: %d/%d completed (%d%%)"
                   completed total
                   (if (> total 0)
                       (round (* 100 (/ (float completed) total)))
                     0)))
      (message "No daily page for today yet."))))

;;; ----------------------------------------------------------------------------
;;; Statistics
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun org-techo-stats ()
  "Display org-techo statistics."
  (interactive)
  (let* ((dir (expand-file-name "daily" org-techo-directory))
         (files (and (file-directory-p dir)
                     (directory-files dir nil "\\.org$")))
         (count (length files)))
    (message "org-techo: %d daily pages in %s" count org-techo-directory)))

;;; ----------------------------------------------------------------------------
;;; Transient Menu
;;; ----------------------------------------------------------------------------

;;;###autoload (autoload 'org-techo-menu "org-techo" nil t)
(transient-define-prefix org-techo-menu ()
  "Org-techo command menu."
  :info-manual "(org-techo)"
  ["Navigation"
   ("t" "Today" org-techo-goto-today)
   ("y" "Yesterday" org-techo-goto-yesterday)
   ("m" "Tomorrow" org-techo-goto-tomorrow)
   ("d" "Pick date..." org-techo-goto-date)
   ("l" "List all days" org-techo-list-days)]
  ["Habits"
   ("h" "Toggle habit" org-techo-toggle-habit)
   ("s" "Habits summary" org-techo-habits-summary)]
  ["Info"
   ("i" "Statistics" org-techo-stats)
   ("c" "Customize" org-techo-customize)])

;;;###autoload
(defun org-techo-customize ()
  "Open org-techo customization buffer."
  (interactive)
  (customize-group 'org-techo))

;;; ----------------------------------------------------------------------------
;;; Keymap
;;; ----------------------------------------------------------------------------

(defvar org-techo-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'org-techo-goto-today)
    (define-key map (kbd "y") #'org-techo-goto-yesterday)
    (define-key map (kbd "m") #'org-techo-goto-tomorrow)
    (define-key map (kbd "d") #'org-techo-goto-date)
    (define-key map (kbd "l") #'org-techo-list-days)
    (define-key map (kbd "h") #'org-techo-toggle-habit)
    (define-key map (kbd "s") #'org-techo-habits-summary)
    (define-key map (kbd "i") #'org-techo-stats)
    (define-key map (kbd "?") #'org-techo-menu)
    map)
  "Keymap for org-techo commands.
Bind this to a prefix key, e.g.:
  (global-set-key (kbd \"C-c t\") org-techo-command-map)")

(fset 'org-techo-command-map org-techo-command-map)

(provide 'org-techo)
;;; org-techo.el ends here
