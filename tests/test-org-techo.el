;;; test-org-techo.el --- Tests for org-techo -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jwalsh
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT test suite for org-techo.

;;; Code:

(require 'ert)
(require 'org-techo)

;;; ----------------------------------------------------------------------------
;;; Date Functions
;;; ----------------------------------------------------------------------------

(ert-deftest org-techo-test-today-format ()
  "Test that `org-techo-today' returns date in YYYY-MM-DD format."
  (let ((today (org-techo-today)))
    (should (stringp today))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" today))))

(ert-deftest org-techo-test-today-is-current-date ()
  "Test that `org-techo-today' returns the actual current date."
  (let ((today (org-techo-today))
        (expected (format-time-string "%Y-%m-%d")))
    (should (string= today expected))))

;;; ----------------------------------------------------------------------------
;;; File Path Functions
;;; ----------------------------------------------------------------------------

(ert-deftest org-techo-test-daily-file-name-default ()
  "Test daily file name generation with default date."
  (let ((org-techo-directory "/tmp/test-techo"))
    (should (string-match-p
             "/tmp/test-techo/daily/[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$"
             (org-techo-daily-file-name)))))

(ert-deftest org-techo-test-daily-file-name-specific-date ()
  "Test daily file name generation with specific date."
  (let ((org-techo-directory "/tmp/test-techo"))
    (should (string=
             "/tmp/test-techo/daily/2025-01-15.org"
             (org-techo-daily-file-name "2025-01-15")))))

(ert-deftest org-techo-test-daily-file-name-expands-path ()
  "Test that daily file name expands the directory path."
  (let ((org-techo-directory "~/org/techo"))
    (should-not (string-prefix-p "~" (org-techo-daily-file-name)))))

;;; ----------------------------------------------------------------------------
;;; Habits Checklist
;;; ----------------------------------------------------------------------------

(ert-deftest org-techo-test-habits-checklist-format ()
  "Test that habits checklist generates proper org checkbox format."
  (let ((org-techo-daily-habits '("Exercise" "Reading")))
    (should (string= "- [ ] Exercise\n- [ ] Reading"
                     (org-techo--habits-checklist)))))

(ert-deftest org-techo-test-habits-checklist-empty ()
  "Test habits checklist with empty habits list."
  (let ((org-techo-daily-habits '()))
    (should (string= "" (org-techo--habits-checklist)))))

(ert-deftest org-techo-test-habits-checklist-single ()
  "Test habits checklist with single habit."
  (let ((org-techo-daily-habits '("Meditate")))
    (should (string= "- [ ] Meditate" (org-techo--habits-checklist)))))

;;; ----------------------------------------------------------------------------
;;; Template Generation
;;; ----------------------------------------------------------------------------

(ert-deftest org-techo-test-daily-template-contains-date ()
  "Test that daily template contains the date."
  (let ((org-techo-daily-habits '()))
    (let ((template (org-techo--daily-template "2025-01-15" "Wednesday")))
      (should (string-match-p "2025-01-15" template)))))

(ert-deftest org-techo-test-daily-template-contains-day-name ()
  "Test that daily template contains the day name."
  (let ((org-techo-daily-habits '()))
    (let ((template (org-techo--daily-template "2025-01-15" "Wednesday")))
      (should (string-match-p "Wednesday" template)))))

(ert-deftest org-techo-test-daily-template-contains-time-blocks ()
  "Test that daily template contains time block table."
  (let ((org-techo-daily-habits '()))
    (let ((template (org-techo--daily-template "2025-01-15" "Wednesday")))
      (should (string-match-p "| Time  | Activity |" template))
      (should (string-match-p "| 06:00 |" template))
      (should (string-match-p "| 22:00 |" template)))))

(ert-deftest org-techo-test-daily-template-contains-reflection ()
  "Test that daily template contains evening reflection section."
  (let ((org-techo-daily-habits '()))
    (let ((template (org-techo--daily-template "2025-01-15" "Wednesday")))
      (should (string-match-p "\\* Evening Reflection" template))
      (should (string-match-p "What went well\\?" template))
      (should (string-match-p "Gratitude" template)))))

(ert-deftest org-techo-test-daily-template-includes-habits ()
  "Test that daily template includes habits from customization."
  (let ((org-techo-daily-habits '("Exercise" "Journal")))
    (let ((template (org-techo--daily-template "2025-01-15" "Wednesday")))
      (should (string-match-p "- \\[ \\] Exercise" template))
      (should (string-match-p "- \\[ \\] Journal" template)))))

;;; ----------------------------------------------------------------------------
;;; Customization Variables
;;; ----------------------------------------------------------------------------

(ert-deftest org-techo-test-custom-directory-type ()
  "Test that org-techo-directory is a customizable directory."
  (should (custom-variable-p 'org-techo-directory)))

(ert-deftest org-techo-test-custom-habits-type ()
  "Test that org-techo-daily-habits is customizable."
  (should (custom-variable-p 'org-techo-daily-habits)))

;;; ----------------------------------------------------------------------------
;;; Interactive Commands
;;; ----------------------------------------------------------------------------

(ert-deftest org-techo-test-goto-today-is-interactive ()
  "Test that `org-techo-goto-today' is an interactive command."
  (should (commandp 'org-techo-goto-today)))

(ert-deftest org-techo-test-create-daily-page-is-interactive ()
  "Test that `org-techo-create-daily-page' is an interactive command."
  (should (commandp 'org-techo-create-daily-page)))

(ert-deftest org-techo-test-goto-yesterday-is-interactive ()
  "Test that `org-techo-goto-yesterday' is an interactive command."
  (should (commandp 'org-techo-goto-yesterday)))

(ert-deftest org-techo-test-goto-tomorrow-is-interactive ()
  "Test that `org-techo-goto-tomorrow' is an interactive command."
  (should (commandp 'org-techo-goto-tomorrow)))

(ert-deftest org-techo-test-goto-date-is-interactive ()
  "Test that `org-techo-goto-date' is an interactive command."
  (should (commandp 'org-techo-goto-date)))

(ert-deftest org-techo-test-menu-is-interactive ()
  "Test that `org-techo-menu' is an interactive command."
  (should (commandp 'org-techo-menu)))

;;; ----------------------------------------------------------------------------
;;; Date Offset
;;; ----------------------------------------------------------------------------

(ert-deftest org-techo-test-date-offset-yesterday ()
  "Test date offset for yesterday."
  (let ((yesterday (org-techo--date-offset -1)))
    (should (stringp yesterday))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" yesterday))))

(ert-deftest org-techo-test-date-offset-tomorrow ()
  "Test date offset for tomorrow."
  (let ((tomorrow (org-techo--date-offset 1)))
    (should (stringp tomorrow))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" tomorrow))))

(ert-deftest org-techo-test-date-offset-zero ()
  "Test that date offset of 0 returns today."
  (let ((today (org-techo--date-offset 0))
        (expected (format-time-string "%Y-%m-%d")))
    (should (string= today expected))))

;;; ----------------------------------------------------------------------------
;;; Keymap
;;; ----------------------------------------------------------------------------

(ert-deftest org-techo-test-command-map-exists ()
  "Test that the command map exists."
  (should (keymapp org-techo-command-map)))

(ert-deftest org-techo-test-command-map-has-today ()
  "Test that command map has today binding."
  (should (eq (lookup-key org-techo-command-map "t") 'org-techo-goto-today)))

(ert-deftest org-techo-test-command-map-has-menu ()
  "Test that command map has menu binding."
  (should (eq (lookup-key org-techo-command-map "?") 'org-techo-menu)))

(provide 'test-org-techo)
;;; test-org-techo.el ends here
