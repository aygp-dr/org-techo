;;; generate-test-data.el --- Generate deterministic test data -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jwalsh
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Generates deterministic test data for org-techo upgrade path testing.
;; Uses a seeded PRNG for reproducibility.
;;
;; Usage:
;;   emacs --batch -L . -l test-fixtures/generate-test-data.el \
;;         -f org-techo-test-generate-all

;;; Code:

(require 'cl-lib)
(require 'org-techo)

;; Load persona
(load (expand-file-name "test-fixtures/persona-l7-engineer.el") nil t)

;;; ----------------------------------------------------------------------------
;;; Deterministic Random Number Generator
;;; ----------------------------------------------------------------------------

(defvar org-techo-test-rng-state 12345
  "Seed for deterministic random number generation.")

(defun org-techo-test-rng-seed (seed)
  "Initialize RNG with SEED for reproducibility."
  (setq org-techo-test-rng-state seed))

(defun org-techo-test-rng-next ()
  "Return next pseudo-random number (LCG algorithm)."
  (setq org-techo-test-rng-state
        (mod (+ (* 1103515245 org-techo-test-rng-state) 12345)
             (expt 2 31)))
  org-techo-test-rng-state)

(defun org-techo-test-rng-range (min max)
  "Return random integer in range [MIN, MAX]."
  (+ min (mod (org-techo-test-rng-next) (1+ (- max min)))))

(defun org-techo-test-rng-choice (list)
  "Return random element from LIST."
  (nth (mod (org-techo-test-rng-next) (length list)) list))

(defun org-techo-test-rng-bool (&optional probability)
  "Return t with PROBABILITY (default 0.5)."
  (let ((prob (or probability 50)))
    (< (mod (org-techo-test-rng-next) 100) prob)))

;;; ----------------------------------------------------------------------------
;;; Date Utilities
;;; ----------------------------------------------------------------------------

(defun org-techo-test-date-range (start-date days)
  "Generate list of date strings starting from START-DATE for DAYS days."
  (let ((start (date-to-time (concat start-date " 00:00:00")))
        (dates '()))
    (dotimes (i days)
      (let ((current (time-add start (days-to-time i))))
        (push (format-time-string "%Y-%m-%d" current) dates)))
    (nreverse dates)))

(defun org-techo-test-day-of-week (date-str)
  "Return day of week (0=Sun) for DATE-STR."
  (string-to-number
   (format-time-string "%w" (date-to-time (concat date-str " 12:00:00")))))

(defun org-techo-test-is-weekday (date-str)
  "Return t if DATE-STR is a weekday."
  (let ((dow (org-techo-test-day-of-week date-str)))
    (and (> dow 0) (< dow 6))))

(defun org-techo-test-day-name (date-str)
  "Return day name for DATE-STR."
  (format-time-string "%A" (date-to-time (concat date-str " 12:00:00"))))

;;; ----------------------------------------------------------------------------
;;; Content Generators
;;; ----------------------------------------------------------------------------

(defun org-techo-test-gen-french-activity (day-num)
  "Generate French learning activity for DAY-NUM."
  (let ((activity (org-techo-test-rng-choice org-techo-test-persona-french-topics)))
    (cond
     ((string-match-p "Chapter %d" activity)
      (format activity (1+ (mod day-num 27))))
     ((string-match-p "streak day %d" activity)
      (format activity (+ 100 day-num)))
     ((string-match-p ": %s" activity)
      (format activity (org-techo-test-rng-choice org-techo-test-french-verbs)))
     (t activity))))

(defun org-techo-test-gen-engineering-reading (day-num)
  "Generate engineering reading for DAY-NUM."
  (let* ((book (nth (mod day-num (length org-techo-test-persona-engineering-books))
                    org-techo-test-persona-engineering-books))
         (chapter (1+ (mod (org-techo-test-rng-next) 12))))
    (format "%s (%s) - Ch. %d" (car book) (cdr book) chapter)))

(defun org-techo-test-gen-fiction-reading (day-num)
  "Generate fiction reading for DAY-NUM."
  (let* ((book (nth (mod day-num (length org-techo-test-persona-fiction-books))
                    org-techo-test-persona-fiction-books))
         (pages (org-techo-test-rng-range 20 50)))
    (format "%s (%s) - %d pages" (car book) (cdr book) pages)))

(defun org-techo-test-gen-exercise (date-str)
  "Generate exercise for DATE-STR."
  (let* ((dow (org-techo-test-day-of-week date-str))
         ;; Yoga on Mon/Wed/Fri, Cycling on Tue/Thu/Sat
         (exercise-type (if (member dow '(1 3 5)) "Yoga" "Cycling"))
         (variations (cdr (assoc exercise-type org-techo-test-persona-exercises))))
    (format "%s: %s" exercise-type (org-techo-test-rng-choice variations))))

(defun org-techo-test-gen-work-activity ()
  "Generate a work activity."
  (let ((template (org-techo-test-rng-choice org-techo-test-persona-work-activities)))
    (cond
     ((string-match-p "%s implementation" template)
      (format template (car (org-techo-test-rng-choice org-techo-test-persona-projects))))
     ((string-match-p "1:1 with %s" template)
      (format template (org-techo-test-rng-choice org-techo-test-persona-teammates)))
     ((string-match-p "review: %s" template)
      (format template (car (org-techo-test-rng-choice org-techo-test-persona-projects))))
     (t template))))

(defun org-techo-test-gen-time-blocks (date-str is-weekday)
  "Generate time blocks for DATE-STR based on IS-WEEKDAY."
  (let ((blocks '()))
    (if is-weekday
        ;; Weekday schedule
        (setq blocks
              `(("06:00" . "Morning routine")
                ("07:00" . ,(org-techo-test-gen-exercise date-str))
                ("08:00" . "Commute / Breakfast")
                ("09:00" . ,(format "Deep work: %s"
                                    (cdr (org-techo-test-rng-choice
                                          org-techo-test-persona-projects))))
                ("10:00" . "")
                ("11:00" . ,(org-techo-test-gen-work-activity))
                ("12:00" . "Lunch")
                ("13:00" . ,(org-techo-test-gen-work-activity))
                ("14:00" . "")
                ("15:00" . ,(org-techo-test-gen-work-activity))
                ("16:00" . "")
                ("17:00" . "Wind down / Planning")
                ("18:00" . "Dinner")
                ("19:00" . "French practice")
                ("20:00" . "Reading")
                ("21:00" . "Personal time")
                ("22:00" . "Sleep")))
      ;; Weekend schedule
      (setq blocks
            `(("06:00" . "")
              ("07:00" . "Sleep in")
              ("08:00" . "Breakfast")
              ("09:00" . ,(org-techo-test-gen-exercise date-str))
              ("10:00" . "")
              ("11:00" . "French practice")
              ("12:00" . "Lunch")
              ("13:00" . "Personal project / Reading")
              ("14:00" . "")
              ("15:00" . "")
              ("16:00" . "Errands / Social")
              ("17:00" . "")
              ("18:00" . "Dinner")
              ("19:00" . "Reading")
              ("20:00" . "Relaxation")
              ("21:00" . "")
              ("22:00" . "Sleep"))))
    blocks))

(defun org-techo-test-gen-habits-completion (date-str day-num is-weekday)
  "Generate habit completions for DATE-STR on DAY-NUM, IS-WEEKDAY."
  (let ((completions '()))
    ;; French practice - 85% completion rate
    (push (cons "French practice (30 min)" (org-techo-test-rng-bool 85)) completions)
    ;; Engineering reading - 70% on weekdays, 50% weekends
    (push (cons "Engineering reading"
                (org-techo-test-rng-bool (if is-weekday 70 50))) completions)
    ;; Fiction reading - 60% overall
    (push (cons "Fiction reading" (org-techo-test-rng-bool 60)) completions)
    ;; Exercise - 90% on scheduled days (skip Sunday)
    (push (cons "Exercise (yoga/cycling)"
                (and (/= (org-techo-test-day-of-week date-str) 0)
                     (org-techo-test-rng-bool 90))) completions)
    ;; Code review - 95% on weekdays
    (push (cons "Code review"
                (and is-weekday (org-techo-test-rng-bool 95))) completions)
    ;; Deep work - 80% on weekdays
    (push (cons "Deep work block"
                (and is-weekday (org-techo-test-rng-bool 80))) completions)
    (nreverse completions)))

(defun org-techo-test-gen-notes (date-str day-num is-weekday)
  "Generate notes for DATE-STR on DAY-NUM, IS-WEEKDAY."
  (let ((notes '()))
    ;; Add project notes on weekdays
    (when (and is-weekday (org-techo-test-rng-bool 60))
      (let ((project (org-techo-test-rng-choice org-techo-test-persona-projects)))
        (push (format "- %s: %s"
                      (car project)
                      (org-techo-test-rng-choice
                       '("Made progress on core algorithm"
                         "Discussed architecture with team"
                         "Fixed edge case in error handling"
                         "Reviewed PR, suggested improvements"
                         "Updated documentation"
                         "Investigated performance issue")))
              notes)))
    ;; Add French learning notes
    (when (org-techo-test-rng-bool 40)
      (push (format "- French: %s"
                    (org-techo-test-rng-choice
                     '("Learned 10 new vocabulary words"
                       "Grammar finally clicking!"
                       "Struggled with subjunctive mood"
                       "Good conversation practice"
                       "Completed lesson on past tense")))
            notes))
    ;; Add reading notes
    (when (org-techo-test-rng-bool 30)
      (push (format "- Reading: %s"
                    (org-techo-test-rng-choice
                     '("Interesting chapter on distributed systems"
                       "Great plot twist!"
                       "Taking notes on architecture patterns"
                       "Finished the book, highly recommend")))
            notes))
    (if notes
        (mapconcat #'identity notes "\n")
      "")))

(defun org-techo-test-gen-reflection (date-str day-num)
  "Generate evening reflection for DATE-STR on DAY-NUM."
  (let ((went-well (org-techo-test-rng-choice
                    '("Productive deep work session"
                      "Good exercise, feeling energized"
                      "Made progress on French"
                      "Helpful code review discussion"
                      "Finished challenging chapter"
                      "Great team collaboration"
                      "Solved a tricky bug")))
        (improve (org-techo-test-rng-choice
                  '("Need more focus time"
                    "Should sleep earlier"
                    "Spend less time in meetings"
                    "More consistent French practice"
                    "Better work-life balance"
                    "More reading time"
                    "Less context switching")))
        (gratitude (org-techo-test-rng-choice
                    '("Supportive team"
                      "Good health"
                      "Interesting work"
                      "Learning opportunities"
                      "Flexible schedule"
                      "Great weather for cycling"
                      "Progress on goals"))))
    (list went-well improve gratitude)))

;;; ----------------------------------------------------------------------------
;;; Page Generator
;;; ----------------------------------------------------------------------------

(defun org-techo-test-gen-daily-page (date-str day-num)
  "Generate complete daily page for DATE-STR on DAY-NUM."
  ;; Seed RNG based on date for reproducibility
  (org-techo-test-rng-seed (+ 42 (* day-num 1000)
                              (string-to-number (replace-regexp-in-string "-" "" date-str))))

  (let* ((day-name (org-techo-test-day-name date-str))
         (is-weekday (org-techo-test-is-weekday date-str))
         (time-blocks (org-techo-test-gen-time-blocks date-str is-weekday))
         (habits (org-techo-test-gen-habits-completion date-str day-num is-weekday))
         (notes (org-techo-test-gen-notes date-str day-num is-weekday))
         (reflection (org-techo-test-gen-reflection date-str day-num)))

    (concat
     (format "#+TITLE: %s - %s\n" date-str day-name)
     (format "#+DATE: %s\n" date-str)
     "#+STARTUP: overview\n"
     (format "#+PROPERTY: PERSONA %s\n" org-techo-test-persona-name)
     (format "#+PROPERTY: ROLE %s\n\n" org-techo-test-persona-role)

     "* Daily Overview\n"
     "** Intentions\n"
     (format "- [ ] %s\n" (if is-weekday
                              (format "Focus on %s"
                                      (cdr (org-techo-test-rng-choice
                                            org-techo-test-persona-projects)))
                            "Rest and recharge"))
     (format "- [ ] %s\n" (org-techo-test-gen-french-activity day-num))
     (format "- [ ] %s\n\n" (if is-weekday
                                (org-techo-test-gen-engineering-reading day-num)
                              (org-techo-test-gen-fiction-reading day-num)))

     "** Time Blocks\n"
     "| Time  | Activity |\n"
     "|-------+----------|\n"
     (mapconcat (lambda (block)
                  (format "| %s | %s |" (car block) (cdr block)))
                time-blocks "\n")
     "\n\n"

     "* Habits\n"
     (mapconcat (lambda (habit)
                  (format "- [%s] %s"
                          (if (cdr habit) "X" " ")
                          (car habit)))
                habits "\n")
     "\n\n"

     "* Notes\n"
     (if (string-empty-p notes) "" (concat notes "\n"))
     "\n"

     "* Evening Reflection\n"
     "** What went well?\n"
     (format "%s\n\n" (nth 0 reflection))
     "** What could be improved?\n"
     (format "%s\n\n" (nth 1 reflection))
     "** Gratitude\n"
     (format "%s\n" (nth 2 reflection)))))

;;; ----------------------------------------------------------------------------
;;; Batch Generation
;;; ----------------------------------------------------------------------------

(defun org-techo-test-generate-fixtures (start-date num-days output-dir)
  "Generate NUM-DAYS of test fixtures starting from START-DATE to OUTPUT-DIR."
  (let ((dates (org-techo-test-date-range start-date num-days))
        (daily-dir (expand-file-name "daily" output-dir)))
    ;; Ensure directory exists
    (make-directory daily-dir t)

    (let ((day-num 0))
      (dolist (date dates)
        (let ((file-path (expand-file-name (format "%s.org" date) daily-dir))
              (content (org-techo-test-gen-daily-page date day-num)))
          (with-temp-file file-path
            (insert content))
          (message "Generated: %s" file-path))
        (setq day-num (1+ day-num))))

    (message "Generated %d daily pages in %s" num-days daily-dir)))

(defun org-techo-test-generate-all ()
  "Generate all test fixtures (90 days starting 2025-01-01)."
  (interactive)
  (let ((output-dir (expand-file-name "test-fixtures/techo-data"
                                       (file-name-directory
                                        (or load-file-name buffer-file-name
                                            default-directory)))))
    (org-techo-test-generate-fixtures "2025-01-01" 90 output-dir)
    (message "Test fixture generation complete!")))

(defun org-techo-test-generate-sample ()
  "Generate small sample (7 days) for quick testing."
  (interactive)
  (let ((output-dir (expand-file-name "test-fixtures/techo-sample"
                                       (file-name-directory
                                        (or load-file-name buffer-file-name
                                            default-directory)))))
    (org-techo-test-generate-fixtures "2025-01-01" 7 output-dir)
    (message "Sample fixture generation complete!")))

(provide 'generate-test-data)
;;; generate-test-data.el ends here
