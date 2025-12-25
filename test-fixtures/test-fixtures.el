;;; test-fixtures.el --- Validation tests for test fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jwalsh
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Validation tests for org-techo test fixtures.
;; These tests verify fixture integrity for upgrade path testing.
;;
;; Usage:
;;   emacs --batch -L . -l test-fixtures/test-fixtures.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-techo)

(defvar org-techo-test-fixture-dir
  (expand-file-name "techo-data"
                    (file-name-directory (or load-file-name buffer-file-name
                                             default-directory)))
  "Directory containing test fixtures.")

(defvar org-techo-test-sample-dir
  (expand-file-name "techo-sample"
                    (file-name-directory (or load-file-name buffer-file-name
                                             default-directory)))
  "Directory containing sample fixtures.")

;;; ----------------------------------------------------------------------------
;;; Fixture Integrity Tests
;;; ----------------------------------------------------------------------------

(ert-deftest org-techo-fixture-test-sample-exists ()
  "Test that sample fixture directory exists."
  (should (file-directory-p org-techo-test-sample-dir)))

(ert-deftest org-techo-fixture-test-sample-count ()
  "Test that sample has exactly 7 daily files."
  (let* ((daily-dir (expand-file-name "daily" org-techo-test-sample-dir))
         (files (directory-files daily-dir nil "\\.org$")))
    (should (= 7 (length files)))))

(ert-deftest org-techo-fixture-test-full-exists ()
  "Test that full fixture directory exists."
  (should (file-directory-p org-techo-test-fixture-dir)))

(ert-deftest org-techo-fixture-test-full-count ()
  "Test that full fixture has exactly 90 daily files."
  (let* ((daily-dir (expand-file-name "daily" org-techo-test-fixture-dir))
         (files (directory-files daily-dir nil "\\.org$")))
    (should (= 90 (length files)))))

;;; ----------------------------------------------------------------------------
;;; Content Validation Tests
;;; ----------------------------------------------------------------------------

(ert-deftest org-techo-fixture-test-file-structure ()
  "Test that fixture files have correct org structure."
  (let ((file (expand-file-name "daily/2025-01-01.org" org-techo-test-sample-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      ;; Check for required sections
      (should (string-match-p "^#\\+TITLE:" (buffer-string)))
      (should (string-match-p "^#\\+DATE:" (buffer-string)))
      (should (string-match-p "^\\* Daily Overview" (buffer-string)))
      (should (string-match-p "^\\* Habits" (buffer-string)))
      (should (string-match-p "^\\* Notes" (buffer-string)))
      (should (string-match-p "^\\* Evening Reflection" (buffer-string))))))

(ert-deftest org-techo-fixture-test-persona-properties ()
  "Test that fixture files have persona properties."
  (let ((file (expand-file-name "daily/2025-01-01.org" org-techo-test-sample-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (string-match-p "^#\\+PROPERTY: PERSONA" (buffer-string)))
      (should (string-match-p "^#\\+PROPERTY: ROLE" (buffer-string))))))

(ert-deftest org-techo-fixture-test-habits-format ()
  "Test that habits have correct checkbox format."
  (let ((file (expand-file-name "daily/2025-01-01.org" org-techo-test-sample-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      ;; Should have habits with checkboxes
      (should (string-match-p "^- \\[[ X]\\] " (buffer-string))))))

(ert-deftest org-techo-fixture-test-time-blocks ()
  "Test that time blocks table exists."
  (let ((file (expand-file-name "daily/2025-01-01.org" org-techo-test-sample-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (string-match-p "| Time  | Activity |" (buffer-string)))
      (should (string-match-p "| 06:00 |" (buffer-string)))
      (should (string-match-p "| 22:00 |" (buffer-string))))))

;;; ----------------------------------------------------------------------------
;;; Determinism Tests
;;; ----------------------------------------------------------------------------

(ert-deftest org-techo-fixture-test-deterministic ()
  "Test that fixture generation is deterministic."
  ;; Generate same date twice with same seed
  (load (expand-file-name "test-fixtures/generate-test-data.el") nil t)
  (let ((content1 (org-techo-test-gen-daily-page "2025-01-15" 14))
        (content2 (org-techo-test-gen-daily-page "2025-01-15" 14)))
    (should (string= content1 content2))))

;;; ----------------------------------------------------------------------------
;;; Statistics Validation
;;; ----------------------------------------------------------------------------

(defun org-techo-fixture-count-habits (dir)
  "Count total and completed habits across all files in DIR."
  (let ((total 0)
        (completed 0)
        (daily-dir (expand-file-name "daily" dir)))
    (dolist (file (directory-files daily-dir t "\\.org$"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^- \\[\\([ X]\\)\\]" nil t)
          (setq total (1+ total))
          (when (string= (match-string 1) "X")
            (setq completed (1+ completed))))))
    (cons completed total)))

(ert-deftest org-techo-fixture-test-habit-stats ()
  "Test that habit statistics are reasonable."
  (let* ((stats (org-techo-fixture-count-habits org-techo-test-sample-dir))
         (completed (car stats))
         (total (cdr stats))
         (rate (if (> total 0) (/ (* 100.0 completed) total) 0)))
    ;; Should have some habits
    (should (> total 0))
    ;; Completion rate should be between 30-90%
    (should (> rate 30))
    (should (< rate 90))))

(ert-deftest org-techo-fixture-test-full-habit-count ()
  "Test habit count in full fixture set."
  (let* ((stats (org-techo-fixture-count-habits org-techo-test-fixture-dir))
         (total (cdr stats)))
    ;; 90 days * (6 habits + 3 intentions) = 810 total checkboxes
    (should (= total 810))))

;;; ----------------------------------------------------------------------------
;;; Date Validation
;;; ----------------------------------------------------------------------------

(ert-deftest org-techo-fixture-test-date-sequence ()
  "Test that dates are sequential in sample."
  (let* ((daily-dir (expand-file-name "daily" org-techo-test-sample-dir))
         (files (sort (directory-files daily-dir nil "\\.org$") #'string<)))
    (should (string= (car files) "2025-01-01.org"))
    (should (string= (car (last files)) "2025-01-07.org"))))

(ert-deftest org-techo-fixture-test-weekday-detection ()
  "Test that weekdays are correctly identified."
  ;; 2025-01-01 is Wednesday, 2025-01-04 is Saturday
  (let ((wed-file (expand-file-name "daily/2025-01-01.org" org-techo-test-sample-dir))
        (sat-file (expand-file-name "daily/2025-01-04.org" org-techo-test-sample-dir)))
    (with-temp-buffer
      (insert-file-contents wed-file)
      ;; Wednesday should have work activities
      (should (string-match-p "Deep work:" (buffer-string))))
    (with-temp-buffer
      (insert-file-contents sat-file)
      ;; Saturday should have "Sleep in"
      (should (string-match-p "Sleep in" (buffer-string))))))

;;; ----------------------------------------------------------------------------
;;; Upgrade Path Tests
;;; ----------------------------------------------------------------------------

(defvar org-techo-fixture-checksums
  '(("2025-01-01.org" . "v1")
    ("2025-01-15.org" . "v1")
    ("2025-02-01.org" . "v1")
    ("2025-03-01.org" . "v1"))
  "Expected checksums for key fixture files (version tracking).")

(ert-deftest org-techo-fixture-test-key-files-exist ()
  "Test that key milestone files exist."
  (dolist (entry org-techo-fixture-checksums)
    (let ((file (expand-file-name (concat "daily/" (car entry))
                                   org-techo-test-fixture-dir)))
      (should (file-exists-p file)))))

(provide 'test-fixtures)
;;; test-fixtures.el ends here
