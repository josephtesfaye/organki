;; -*- lexical-binding: t; -*-
;; Copyright (C) 2024 josephtesfaye

;; Author: Joseph Huang
;; URL: https://josephtesfaye.com

;;; Commentary:
;; This file is for testing Organki.

;;; Code:
(require 'ert)
(require 'eieio)
(require 'jts-org-utils)
(require 'jts-buffer-utils)
(require 'organki)

(ert-deftest test-organki/default-values ()
  "Default value via in-buffer settings"
  :tags '(organki)

  (test-organki--call-import-region-on-case "20250806211938"))


;; Utilities
(defconst test-file
  (let* ((resources (expand-file-name "test/resources" (project-root (project-current))))
         (file (expand-file-name "test-organki-2.org" resources)))
    (find-file-noselect file))
  "Get the test file buffer.")


(defun test-organki--call-import-region-on-case (name)
  (with-current-buffer test-file
    (elog--debug "Test Name: %s" name)
    (let* ((element (test-organki--get-element-by-name name))
           (content (oref element content))
           (attr-args (oref element attr-args))
           (points (plist-get attr-args :points))
           (regions (plist-get attr-args :regions)))
      ;; Put the current point at the specified points and call import-region
      ;; respectively.
      (dolist (point points)
        (elog--debug "Test point: %s" point)
        (goto-char point)
        (deactivate-mark)
        (test-organki--call-import-region attr-args content))
      ;; Select the regions at the specified points and call import-region
      ;; respectively.
      (dolist (region regions)
        (elog--debug "Test region: %s" region)
        (select-region (car region) (cadr region))
        (test-organki--call-import-region attr-args content))
      (deactivate-mark))))


(defun test-organki--get-element-by-name (name)
  "Get an element by its name."

  (let ((data (org-element-parse-buffer)))
    (org-element-map data 'example-block
      (lambda (element)
        (when (string= name (org-element-property :name element))
          (org-example-convert element)))
      nil t)))


(defun test-organki--call-import-region (attr-args content)
  (let ((result-file nil)
        (result-buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (prompt &optional initial-input history default-value)
                     (cond ((string-match "Notetype" prompt)
                            ;; Here you can reference variables in
                            ;; `organki/import-region', for example,
                            ;; `attr-anki'. If there's a variable also named
                            ;; `attr-args' in `organki/import-region' it would
                            ;; override this one here.
                            (or (plist-get attr-args :notetype) default-value))
                           ((string-match "Deck" prompt)
                            (or (plist-get attr-args :deck) default-value))
                           ((string-match "Tags" prompt)
                            (or (plist-get attr-args :tags) default-value))
                           (t default-value))))
                  ((symbol-function 'read-directory-name)
                   (lambda (prompt &optional dir default-dirname mustmatch initial)
                     default-dirname))
                  (outputs nil))

          ;; Call the function to test
          (setq outputs (call-interactively 'organki/import-region))
          ;; `outputs' is an alist containing the output filepaths like this:
          ;; ((vocabulary . "/Users/josephtesfaye/Downloads/import20241026160048-cb937b3f3fa1.txt"))
          (setq result-file (cdar outputs))
          (setq result-buffer (find-file-noselect result-file))
          (should (equal (with-current-buffer result-buffer
                           (goto-char (point-max))
                           (buffer-substring-no-properties (point-min) (point-max)))
                         content)))

      ;; Delete the outputs buffer and file
      (with-current-buffer result-buffer
        (save-buffer)
        (should (kill-buffer)))
      (delete-file result-file nil)
      ;; Delete from recent files
      (setq recentf-list (delete (abbreviate-file-name result-file) recentf-list)))))
