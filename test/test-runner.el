;; Copyright (C) 2024 Joseph Huang

;; Author: Joseph Huang
;; URL: https://josephtesfaye.com

;;; Commentary:
;; This file is for running tests in batch for the current project.

;;; Code:
(require 'jts-test-utils)

(defun test-runner ()
  (interactive)
  (cl-block nil
    (let ((test-root (expand-file-name "test" (project-root (project-current)))))
      ;; Run tests for organki
      (test-run-tests
       test-root
       '("test-organki.el" "test-organki-2.el")
       '(tag organki)))))

(test-runner)
