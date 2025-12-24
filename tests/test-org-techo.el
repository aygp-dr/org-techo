;;; test-org-techo.el --- Tests for org-techo -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-techo)

(ert-deftest test-org-techo-today ()
  "Test that today's date is returned in correct format."
  (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$"
                          (org-techo-today))))

(ert-deftest test-org-techo-daily-file-name ()
  "Test daily file name generation."
  (let ((org-techo-directory "/tmp/test-techo"))
    (should (string-match-p "/tmp/test-techo/daily/[0-9-]+\\.org$"
                            (org-techo-daily-file-name)))))

;;; test-org-techo.el ends here
