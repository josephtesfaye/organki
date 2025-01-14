;; -*- lexical-binding: t; -*-
;; Copyright (C) 2024 josephtesfaye

;; Author: Joseph Tesfaye
;; URL: https://josephtesfaye.com

;;; Commentary:
;; This file is for testing Organki.

;;; Code:
(require 'ert)
(require 'eieio)
(require 'jts-org-utils)
(require 'jts-buffer-utils)
(require 'organki)

;; * Tests for business logics
(defconst test-organki
  (let* ((resources (expand-file-name "test/resources" (project-root (project-current))))
         (file (expand-file-name "test-organki.org" resources)))
    (find-file-noselect file))
  "Get the test file buffer.")


(defconst test-organki-elements
  (let ((table (make-hash-table :test 'equal)))
    (with-current-buffer test-organki
      (let ((name nil))
        (org-element-map (org-element-parse-buffer) 'example-block
          (lambda (current-block)
            (setq name (org-element-property :name current-block))
            (when name
              (puthash name (org-example-convert current-block) table))))))
    table)

  "A constant hash table that stores the examples from the test file.")

;; ==== Test cases ====
(ert-deftest test-organki/import-vocabulary ()
  "Case 1: Converting a Vocabulary list"
  :tags '(organki)

  (with-current-buffer test-organki
    (test-organki/import-region-case "case1/organki/import-region/output1")
    (test-organki/import-region-case "case1/organki/import-region/output2")
    (test-organki/import-region-case "case1/organki/import-region/output3")))

(defun test-organki/import-region-case (output-name)
  (elog--debug "Test Name: %s" output-name)
  (let* ((element (gethash output-name test-organki-elements))
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
      (test-organki/call-import-region attr-args content))
    ;; Select the regions at the specified points and call import-region
    ;; respectively.
    (dolist (region regions)
      (elog--debug "Test region: %s" region)
      (select-region (car region) (cadr region))
      (test-organki/call-import-region attr-args content))
    (deactivate-mark)))


(defun test-organki/call-import-region (attr-args content)
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


(ert-deftest test-organki/import-sentences ()
  "Case 2: Converting a Sentence list"
  :tags '(organki)

  (with-current-buffer test-organki
    (test-organki/import-region-case "case2/organki/import-region/output1")
    (test-organki/import-region-case "case2/organki/import-region/output2")
    (test-organki/import-region-multi-outputs "case2/organki/import-region/output3")
    (test-organki/import-region-case "case2/organki/import-region/output4")
    (test-organki/import-region-multi-outputs "case2/organki/import-region/output5")
    (test-organki/import-region-multi-outputs "case2/organki/import-region/output6")
    (test-organki/import-region-multi-outputs "case2/organki/import-region/output7")
    (test-organki/import-region-multi-outputs "case2/organki/import-region/output8")
    (test-organki/import-region-multi-outputs "case2/organki/import-region/output9")))


(defun test-organki/import-region-multi-outputs (output-name)
  (elog--debug "Test Name: %s" output-name)
  (let* ((attr-args nil)
         (attr-type nil)
         (points nil)
         (regions nil)
         (outputs nil))

    (maphash (lambda (key element)
               (when (string-prefix-p output-name key)
                 (when (oref element attr-args)
                   (setq attr-args (oref element attr-args))
                   (setq points (plist-get attr-args :points))
                   (setq regions (plist-get attr-args :regions)))
                 (setq attr-type (org-element-property :attr_type (oref element org-block)))
                 (push (cons (intern (car attr-type)) (oref element content)) outputs)))
             test-organki-elements)

    ;; Put the current point at the specified points and call import-region
    ;; respectively.
    (dolist (point points)
      (elog--debug "Test point: %s" point)
      (goto-char point)
      (deactivate-mark)
      (test-organki/call-import-region-multi-outputs attr-args outputs))
    ;; Select the regions at the specified points and call import-region
    ;; respectively.
    (dolist (region regions)
      (elog--debug "Test region: %s" region)
      (select-region (car region) (cadr region))
      (test-organki/call-import-region-multi-outputs attr-args outputs))
    (deactivate-mark)))


(defun test-organki/call-import-region-multi-outputs (attr-args
                                                      example-outputs)
  (let (results)
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
                     default-dirname)))

          ;; Call the function to test
          (setq results (call-interactively 'organki/import-region))
          (dolist (result results)
            (let* ((result-file (cdr result))
                   (result-buffer (find-file-noselect result-file))
                   (example-content (alist-get (car result) example-outputs)))
              (should (equal (with-current-buffer result-buffer
                               (goto-char (point-max))
                               (buffer-substring-no-properties (point-min) (point-max)))
                             example-content)))))

      ;; Delete the results buffer and file
      (dolist (result results)
        (let* ((result-file (cdr result))
               (result-buffer (find-file-noselect result-file)))
          (with-current-buffer result-buffer
            (save-buffer)
            (should (kill-buffer)))
          (delete-file result-file nil)
          ;; Delete from recent files
          (setq recentf-list (delete (abbreviate-file-name result-file) recentf-list)))))))


(ert-deftest test-organki/default-properties ()
  "Case 3: Converting default properties"
  :tags '(organki)
  (with-current-buffer test-organki
    (test-organki/import-region-case "case3/default-properties/vocabulary")
    (test-organki/import-region-case "case3/default-properties/sentence")))


(ert-deftest test-organki/documentation-examples ()
  "Case 4: Testing documentation examples"
  :tags '(organki)

  (let* ((doc-examples (make-hash-table :test 'equal))
         (output-element (gethash "case4/documentation/default-properties"
                                  test-organki-elements))
         (output-content (oref output-element content))
         (attr-args (oref output-element attr-args))
         (points (plist-get attr-args :points)))
    (with-current-buffer (find-file-noselect
                          (expand-file-name "readme.org"
                                            (project-root (project-current))))
      (org-element-map (org-element-parse-buffer) 'example-block
        (lambda (current-block)
          (let (name)
            (setq name (org-element-property :name current-block))
            (when (and name (string-prefix-p "organki" name))
              (puthash name (org-example-convert current-block) doc-examples))))))

    (with-temp-buffer
      (insert (string-replace
               "\\*" "*"
               (oref (gethash "organki-example-properties" doc-examples) content)))
      (org-mode)
      (dolist (point points)
        (elog--debug "Testing point: %s" point)
        (goto-char point)
        (deactivate-mark)
        (test-organki/call-import-region attr-args output-content)))))


(ert-deftest test-organki/vocabulary-sublists ()
  "Case5: Testing comments, examples for Vocabulary"
  :tags '(organki)

  (with-current-buffer test-organki
    (test-organki/import-region-case "case5/vocabulary/sublists/1")
    (test-organki/import-region-case "case5/vocabulary/sublists/2")
    (test-organki/import-region-multi-outputs "case5/vocabulary/sublists/3")
    (test-organki/import-region-multi-outputs "case5/vocabulary/sublists/4")))


(ert-deftest test-organki/prettfy-region ()
  "Case 6: Testing prettifying region"
  :tags '(organki)

  (with-current-buffer test-organki
    (let* ((element (gethash "case6/prettify-region" test-organki-elements))
           (content (oref element content))
           (attr-args (oref element attr-args))
           (points (plist-get attr-args :points)))
      (dolist (point points)
        (elog--debug "Testing point: %s" point)
        (goto-char point)
        (should (equal (organki--sentences-prettified-string) content))))))


(ert-deftest test-organki/multi-entry-lines ()
  "Case 7: Supporting multiple entry lines for Sentence lists"
  :tags '(organki)

  (with-current-buffer test-organki
    (test-organki/import-region-multi-outputs "case7/sentence/multi/")
    (test-organki/import-region-multi-outputs "case7/sentence/multi-apr/")
    (test-organki/import-region-multi-outputs "case7/sentence/multi-sep/")))


;; * Unit Tests
(ert-deftest test-organki--convert-pronunciation ()
  :tags '(organki)

  (should (equal (organki--convert-pronunciation "test [[cl:202410021528.m4a][⏯]]")
                 "test [sound:202410021528.m4a]"))
  (should (equal (organki--convert-pronunciation "test [[cl:abc][⏯]]")
                 "test [sound:abc]"))
  (should (equal (organki--convert-pronunciation "test 123")
                 "test 123"))
  (should-error (organki--convert-pronunciation nil)))
