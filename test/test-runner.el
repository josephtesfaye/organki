;; Copyright (C) 2024 Joseph Huang

;; Author: Joseph Huang
;; URL: https://josephtesfaye.com

;;; Commentary:
;; This file is for running tests in batch for the current project.

;;; Code:
(setq test-root (expand-file-name "test" (project-root (project-current))))
(add-to-list 'load-path test-root)

(require 'jhelt)

(defun test-runner ()
  (interactive)
  (cl-block nil
    (jhelt/run-tests test-root
                    '("organki-test.el")
                    '(tag organki))))

(test-runner)
