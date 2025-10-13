;; -*- lexical-binding: t; -*-
;; Copyright (C) 2024 josephtesfaye

;; Author: Joseph Huang
;; URL: https://josephtesfaye.com

;;; Commentary:
;; This file is for testing Organki.

;;; Code:
(require 'organki-test-common)

;; * Auto Tests
(ert-deftest organki-test/run-auto-tests ()
  "Run all automatic tests in the supplied files."
  :tags '(organki)

  (organki-test--run-files
   '("organki-test.org"
     "organki-test-2.org")))


;; * Unit Tests
(ert-deftest organki-test/convert-pronunciation ()
  :tags '(organki)

  (should (equal (organki--convert-pronunciation "test [[cl:202410021528.m4a][⏯]]")
                 "test [sound:202410021528.m4a]"))
  (should (equal (organki--convert-pronunciation "test [[cl:abc][⏯]]")
                 "test [sound:abc]"))
  (should (equal (organki--convert-pronunciation "test 123")
                 "test 123")))


(ert-deftest organki-test/convert-vocabulary-body-function ()
  "Test `organki/convert-vocabulary-body-function'."
  :tags '(organki)

  (with-current-buffer (find-file-noselect (organki-test--find-file
                                            "organki-test-2.org" 'test))
    (elog--debug "* Run test organki/convert-vocabulary-body-function in file: %s"
                 (buffer-name))
    (let ((organki/convert-vocabulary-body-function
           #'organki--convert-vocabulary-body-old))
      (organki-test--run-example "TOKYO20250924093055"))
    (organki-test--run-example "TOKYO20250926230233")))


(ert-deftest organki-test/organki-test--get-tests ()
  "Test `organki-test--get-tests'."
  :tags '(organki)

  (with-current-buffer (find-file-noselect (organki-test--find-file
                                            "organki-test-2.org" 'test))
    (elog--debug "* Run test organki-test--get-tests in file: %s" (buffer-name))
    (let ((io-list (organki-test--get-tests "Test ~organki-test--get-tests~" t))
          (sub-buffer "*organki-test--get-tests*")
          output expect)
      (dolist (io io-list)
        (elog--debug "Run test in %s at point %s" (current-buffer) (car io))
        (goto-char (car io))
        (deactivate-mark)
        (org-edit-src-code nil sub-buffer)
        (with-current-buffer sub-buffer
          (setq output (organki-test--get-tests 1))
          (org-edit-src-abort))
        (setq expect (organki-test--output :points (cadr io)))
        (setq expect (oref (car (oref expect elements)) content))
        (setq expect (read expect))
        (ert-info ((format (concat "Output doesn't match expect at %s in %s.\n"
                                   ">> Output:\n%s\n"
                                   ">> Expect:\n%s\n")
                           (car io) (current-buffer) output expect)
                   :prefix "Error: ")
          (should (equal output expect)))))))


(ert-deftest organki-test/prettfy-region ()
  :tags '(organki)

  (with-current-buffer (find-file-noselect (organki-test--find-file
                                            "organki-test.org" 'test))
    (elog--debug "* Run test prettfy-region in file: %s" (buffer-name))
    (let* ((io (car (organki-test--get-tests "Testing prettifying region" t)))
           (input (car io))
           (output (car (oref (organki-test--output :points (cadr io)) elements)))
           (content (oref output content)))
      (elog--debug "prettfy-region: Run test at point %s" input)
      (goto-char input)
      (organki-test--compare
       (organki--sentences-prettified-string) content input))))
