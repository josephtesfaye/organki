;; Copyright (C) 2024 Joseph Huang

;; Author: Joseph Huang
;; URL: https://josephtesfaye.com

;;; Commentary:
;; This file is for running tests in batch for the current project.

;;; Code:
(setq test-root (expand-file-name "test" (project-root (project-current))))
(add-to-list 'load-path test-root)
(add-to-list 'load-path "~/projects/jts-utils")

(require 'jts-test-utils)

(defun test-runner ()
  (interactive)
  (cl-block nil
    (test-run-tests test-root
                    '("organki-test.el")
                    '(tag organki))))

(test-runner)
