;;; persona-l7-engineer.el --- Test persona: L7 Engineer -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jwalsh
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Test persona for an L7 engineer balancing:
;; - Technical work (distributed systems project)
;; - Language learning (French)
;; - Reading (engineering + fiction)
;; - Fitness (yoga, cycling)
;;
;; This configuration is deterministic for upgrade path testing.

;;; Code:

(defvar org-techo-test-persona-name "Alex Chen"
  "Name of the test persona.")

(defvar org-techo-test-persona-role "L7 Staff Engineer"
  "Role of the test persona.")

(defvar org-techo-test-persona-habits
  '("French practice (30 min)"
    "Engineering reading"
    "Fiction reading"
    "Exercise (yoga/cycling)"
    "Code review"
    "Deep work block")
  "Daily habits for the L7 engineer persona.")

(defvar org-techo-test-persona-projects
  '(("project-atlas" . "Distributed consensus implementation")
    ("project-babel" . "Service mesh migration")
    ("tech-debt" . "Legacy system modernization"))
  "Work projects for the test persona.")

(defvar org-techo-test-persona-french-topics
  '("Passé composé exercises"
    "Le Petit Prince - Chapter %d"
    "Duolingo streak day %d"
    "French podcast: InnerFrench"
    "Anki flashcards review"
    "Conjugation practice: %s"
    "Watching 'Lupin' with subtitles")
  "French learning activities.")

(defvar org-techo-test-persona-engineering-books
  '(("Designing Data-Intensive Applications" . "Kleppmann")
    ("Database Internals" . "Petrov")
    ("The Art of PostgreSQL" . "Fontaine")
    ("Systems Performance" . "Gregg")
    ("Building Microservices" . "Newman"))
  "Engineering books for reading.")

(defvar org-techo-test-persona-fiction-books
  '(("Project Hail Mary" . "Weir")
    ("The Three-Body Problem" . "Liu")
    ("Neuromancer" . "Gibson")
    ("Snow Crash" . "Stephenson")
    ("Piranesi" . "Clarke"))
  "Fiction books for reading.")

(defvar org-techo-test-persona-exercises
  '(("Yoga" . ("Morning flow" "Vinyasa class" "Yin yoga" "Power yoga"))
    ("Cycling" . ("20km morning ride" "Hill intervals" "Recovery ride" "Long weekend ride")))
  "Exercise types and variations.")

(defvar org-techo-test-persona-work-activities
  '("Design review: %s"
    "PR review: %s implementation"
    "1:1 with %s"
    "Architecture discussion"
    "Incident postmortem"
    "Sprint planning"
    "Tech spec writing"
    "Mentoring session"
    "Performance optimization"
    "Documentation update")
  "Work activities template.")

(defvar org-techo-test-persona-teammates
  '("Jordan" "Sam" "Morgan" "Casey" "Riley" "Quinn")
  "Teammate names for work activities.")

(defvar org-techo-test-french-verbs
  '("être" "avoir" "faire" "aller" "pouvoir" "vouloir" "devoir" "savoir")
  "French verbs for conjugation practice.")

(provide 'persona-l7-engineer)
;;; persona-l7-engineer.el ends here
