;; -*- lexical-binding: t; -*-
;; Copyright (C) 2024 josephtesfaye

;; Author: Joseph Huang
;; URL: https://josephtesfaye.com

;;; Commentary:
;; This file contains common codes for testing Organki.

;;; Code:
(require 'ert)
(require 'eieio)
(require 'organki)

;; Common parameters
(defconst organki-test--bindings
  '(:vbody1
    (organki/convert-vocabulary-body-function
     organki--convert-vocabulary-body-old)
    :vfields1
    (organki/anki-vocabulary-fields
     (entry pronunciation class translation notes)))

  "A plist which contains multiple sets of variable bindings. Users can select a
set of bindings by providing the key via the `:bindings' keyword in the
`#+ATTR_ARGS' line above an output, such as `#+ATTR_ARGS: :bindings :old'. Then
the automatic tests will declare those bindings before calling
`organki/import-region'. This is mainly for testing backward compatibility with
the old test cases as the program evolves.")


(defclass organki-test--output ()
  ((points
    :initarg :points
    :initform nil
    :type (or integer list)
    :documentation
    "The positions of the output blocks. If it's an integer it means the input
has only one output content from a single call to Organki, and if a list of
integers, which is a composite output, the input has one or more output
contents. If the output has inputs from the :inputs key they are also put in a
list where the CAR is the output and the `cadr' is the inputs, such as
'(1 (2 3)) or '((1 2) (3 4)).")

   (elements
    :initarg :outputs
    :initform nil
    :type list
    :documentation
    "All the outputs produced by a single call to Organki represented as a list
of `org-element-wrapper' objects corresponding to `points.' This is to wrap a
composite output in one go and provide convenience for calculating and
retrieving common attributes. A non-composite output is also presented with the
same form.")

   (attr_args
    :initarg :attr_args
    :initform nil
    :type list
    :documentation
    "The #+ATTR_ARGS of the outputs.")

   (inputs
    :initarg :inputs
    :initform nil
    :type list
    :documentation
    "The inputs of the current output from the :inputs key."))

  :documentation "A class used to represent a test output.")


(cl-defmethod initialize-instance :after ((obj organki-test--output)
                                          &rest initargs)
  (let* ((points-old (oref obj points))
         (points (pcase points-old
                   ((pred integerp) (list points-old))
                   ;; has :inputs key
                   ((and (pred listp)
                         (guard (= 2 (length points-old)))
                         (let inputs (cadr points-old))
                         (guard (listp inputs)))
                    (oset obj inputs inputs)
                    (list (car points-old)))
                   ;; are all integers
                   ((and (pred listp)
                         (guard (not (seq-find (lambda (o)
                                                 (not (integerp o)))
                                               points-old))))
                    points-old)))
         (elements (mapcar (lambda (point)
                             (org-element-wrapper
                              :element
                              (org-element-at-point point)))
                           points)))
    (oset obj elements elements)
    ;; Use the the first output's attr_args as the common attr_args of a
    ;; composite output.
    (oset obj attr_args (oref (car elements) attr_args))))


(defun organki-test--run-files (files)
  "Run all runnable tests in FILES automatically. Each element of FILES can
either be a filename—all tests in that file will run, or a list where
the CAR is the filename and the CDR is the headlines—only tests of which will
run."

  (dolist (file files)
    (with-current-buffer (find-file-noselect
                          (organki-test--find-file
                           (if (listp file) (car file) file) 'test))
      (elog--debug "* Run tests in file: %s"
                   (if (listp file)
                       (concat (car file) ": " (string-join (cdr file) ", "))
                     file))
      (let ((headlines (and (listp file) (cdr file))))
        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (element)
            (let* ((headline (org-element-property :raw-value element))
                   (pos (org-element-property :begin element))
                   (tags (org-element-property :tags element)))
              ;; When the headline isn't tagged "noauto" and can be found in
              ;; `headlines' (if non-nil) run it as an automatic test.
              (when (and (not (member "noauto" tags))
                         (or (null headlines) (member headline headlines)))
                (elog--debug "** Run tests in subtree: %s" headline)
                (organki-test--run-subtree pos)))))))))


(defun organki-test--find-file (name &optional dir-type)
  "Find a file by NAME, which can be an absolute file path, or a name relative
to the project root or another directory according to DIR-TYPE."

  (let ((dir (pcase dir-type
               ('test (expand-file-name "test/resources"
                                        (project-root (project-current))))
               ('root (project-root (project-current))))))
    (when (not (file-name-absolute-p name))
      (setq name (expand-file-name name dir)))
    (should (file-exists-p name))
    name))


(defun organki-test--run-subtree (headline-or-pos)
  "Run tests in the subtree of the title given by HEADLINE-OR-POS if it's a
string, or the subtree at position HEADLINE-OR-POS if it's an integer,
assuming the tests are written in the format as described by
`organki-test--get-tests'."

  (let ((io-list (organki-test--get-tests headline-or-pos)))
    (ert-info ((format "No tests found in the subtree at %s in %s"
                       headline-or-pos (current-buffer))
               :prefix "Error: ")
      (should io-list))

    (dolist (io io-list)
      (ert-info ((format "No outputs found for the input at %s in %s"
                         (car io) (current-buffer))
                 :prefix "Error: ")
        (should (cdr io)))

      (let ((outputs (organki-test--get-outputs io)))
        (organki-test--run-outputs (car io) outputs)))))


(defun organki-test--get-outputs (io)
  (let (outputs)
    (dolist (o (cdr io))
      (let* ((output (organki-test--output :points o))
             (attr_args (oref output attr_args))
             (points (plist-get attr_args :points))
             (regions (plist-get attr_args :regions)))
        ;; When no points or regions specified use input’s :begin point.
        (when (not (or points regions))
          (oset output attr_args
                (plist-put attr_args :points (list (car io)))))
        (push output outputs)))
    (nreverse outputs)))


(defun organki-test--run-outputs (input outputs)
  "Run OUTPUTS against the content at INPUT (a point or cons cell)."

  (let* ((input-point (or (and (consp input) (cdr input)) input))
         (element (org-element-at-point-no-context input-point))
         (sub-buffer (format "*organki-test-input-%s*" input-point))
         (main-buffer (current-buffer))
         default-props)
    (if (eq (org-element-type element) 'src-block)
        ;; When the input is a source block
        (save-excursion
          (goto-char input-point)
          (deactivate-mark)
          (org-edit-src-code nil sub-buffer)
          (with-current-buffer sub-buffer
            (dolist (output outputs)
              (organki-test--run-attr_args-with-bindings output)
              (when (and (integerp input) (oref output inputs))
                (organki-test--run-inputs output main-buffer)))
            (org-edit-src-abort)))

      ;; When it's a plain list

      ;; You can't refer to a plain list as an input in the :inputs key since
      ;; the :points and :regions are originally absolute positions for the
      ;; default plain list. This can only be improved when they are also
      ;; changed to relative ones (like for source blocks), which would require
      ;; lots of changes to this code.

      ;; When input is from the :inputs key modify the output's :points to that
      ;; of the input, remove :regions, and use the default input's default
      ;; properties, so that the eventual outputs are the same wherever the
      ;; input is.

      (save-excursion
        (goto-char input-point)
        (deactivate-mark)
        (setq default-props (organki--get-default-properties)))

      (dolist (output outputs)
        (when (consp input)
          (let* ((points (list (cdr input)))
                 (attr_args (plist-put (oref output attr_args) :points points)))
            (oset output attr_args attr_args)))

        (organki-test--run-attr_args-with-bindings output)

        (when (and (integerp input) (oref output inputs))
          (let* ((attr_args (oref output attr_args))
                 (attr_args2 (copy-sequence attr_args)))
            (cl-remf attr_args2 :regions)
            (setq attr_args2 (append attr_args2 default-props))
            (oset output attr_args attr_args2)
            (organki-test--run-inputs output main-buffer)
            (oset output attr_args attr_args)))))))


(defun organki-test--run-inputs (output main-buffer)
  "Run tests against OUTPUT's inputs from the :inputs key."
  (dolist (input (oref output inputs))
    ;; If input is an integer it's a point found in the current file.
    (if (integerp input)
        (with-current-buffer main-buffer
          ;; Pass input in a cons cell to differentiate it from the outer
          ;; input.
          (organki-test--run-outputs (cons nil input) (list output)))

      ;; If it's a cons cell the point is found in another file.
      (with-current-buffer (find-file-noselect (car input))
        (organki-test--run-outputs input (list output))))))


(defun organki-test--run-example (name)
  "Run the test specified by name, which is the value of `#+NAME:' above an
output (example block) in a test file."

  (let* ((output (organki-test--get-element name)))
    (ert-info ((format "No tests found by the name %s in %s"
                       name (current-buffer))
               :prefix "Error: ")
      (should output))
    (organki-test--run-attr_args-with-bindings output)))


(defun organki-test--run-attr_args-with-bindings (output)
  "Run `organki-test--run-attr_args' with the selected variable bindings from
`organki-test--bindings'."

  (let* ((attr_args (oref output attr_args))
         (bindings-key (plist-get attr_args :bindings)))
    (if bindings-key
        (let* ((variables (plist-get organki-test--bindings bindings-key))
               (vars (cl-loop for (k v) on variables by #'cddr collect k))
               (vals (cl-loop for (k v) on variables by #'cddr collect v)))
          (ert-info ((format "No bindings found for the key %s" bindings-key)
                     :prefix "Error: ")
            (should variables))
          (cl-progv vars vals (organki-test--run-attr_args output)))
      (organki-test--run-attr_args output))))


(defun organki-test--run-attr_args (output)
  "Run the test by calling `organki/import-region' against different points and
regions specified by `#+ATTR_ARGS:' line above the OUTPUT."

  (let* ((attr_args (oref output attr_args))
         (points (plist-get attr_args :points))
         (regions (plist-get attr_args :regions))
         (buffer (buffer-name (current-buffer))))
    (should (or points regions))

    ;; Call import-region at specific points respectively.
    (dolist (point points)
      (elog--debug "Run test in %s at point %s" buffer point)
      (goto-char point)
      (deactivate-mark)
      (organki-test--import-region output))

    ;; Call import-region at specific regions respectively.
    (dolist (region regions)
      (elog--debug "Run test in %s in region %s" buffer region)
      (select-region (car region) (cadr region))
      (organki-test--import-region output))
    (deactivate-mark)))


(defun organki-test--get-tests (headline-or-pos &optional ignore-noauto)
  "A test case or scenario is titled with a headline, under which there are
inputs and outputs of the concrete tests. The inputs are written as plain lists
or source blocks, and the outputs of an input are presented in example blocks
following the list until another list.

A subtree, an input, or an output can be marked not for auto testing:

- A subtree is marked with the `noauto' tag in the headline.
- An input is marked with the `noauto' tag in the `#+ATTR_ARGS' line. If an
  input is marked not for testing then all following outputs are also marked as
  such automatically.
- An output is marked with the `:test noauto' pair in the `#+ATTR_ARGS' line.

HEADLINE-OR-POS is either the title or position of such a subtree.

Return an alist where the `car' of each element list is an input's position and
the `cdr' is the positions of the input's corresponding outputs. "

  (let (alist composite-inputs)
    (when-let* ((pos (if (stringp headline-or-pos)
                         (org-find-exact-headline-in-buffer headline-or-pos)
                       headline-or-pos))
                (subtree (org-element-at-point pos))
                ((or ignore-noauto
                     (null (member "noauto" (org-element-property
                                             :tags subtree))))))
      (save-restriction
        (narrow-to-region (org-element-property :contents-begin subtree)
                          (org-element-property :contents-end subtree))
        (org-element-map (org-element-parse-buffer)
            '(plain-list src-block example-block)
          (lambda (element)
            (let* ((begin (org-element-property :begin element))
                   (attr_args (org-element-get-attr-args element))
                   (composite (plist-get attr_args :composite))
                   (noauto (eq 'noauto (plist-get attr_args :test)))
                   inputs)
              ;; If an input is marked with the `noauto' tag in the #+ATTR_ARGS
              ;; line, it is not treated as an input, and the previous unmarked
              ;; list continues to be the current input. If an example block has
              ;; an `:test noauto' it is not treated as an output.
              (if (member (org-element-type element) '(plain-list src-block))
                  (when (not noauto)
                    (push (list begin) alist))

                (cl-symbol-macrolet ((prev-io (car alist)))
                  (when (not noauto)
                    (setq inputs (organki-test--get-inputs attr_args))
                    (ert-info ((format "No input found for the output at %s in %s"
                                       begin (current-buffer))
                               :prefix "Error: ")
                      (should (or prev-io inputs)))

                    (if composite
                        (progn
                          (when (and inputs (null composite-inputs))
                            (setq composite-inputs inputs))
                          (if (listp (car prev-io))
                              (push begin (car prev-io))
                            (push (list begin) prev-io)))

                      ;; An output can have multiple inputs from the :input key.
                      ;; In this case, the output in the return result is a list
                      ;; whose CAR is the output and CDR is the inputs.
                      (when (and (listp (car prev-io)) composite-inputs)
                        (setcar prev-io (list composite-inputs (car prev-io)))
                        (setq composite-inputs nil))

                      ;; For non-composite output
                      (push (if inputs (list inputs begin) begin) prev-io)))))))

          ;; Exclude sublists
          nil nil 'plain-list))
      (setq alist (deep-reverse alist)))
    alist))


(defun organki-test--get-inputs (attr_args)
  "Return the inputs indicated by the :inputs keyword in attr_args as a list of
points. Each point is either an integer (the position of the input in the
current file), or a cons cell whose CAR is the file where the input exists and
CDR the position in that file."

  (let ((inputs (plist-get attr_args :inputs))
        points element)
    (when inputs
      (dolist (input inputs)
        (if-let (((string-match "::" input))
                 (file (substring input 0 (match-beginning 0)))
                 (name (substring input (match-end 0)))
                 (path (if (string-prefix-p "r:" file)
                           (organki-test--find-file
                            (string-remove-prefix "r:" file) 'root)
                         file)))
            (with-current-buffer (find-file-noselect path)
              (setq element (organki-test--get-element name))
              (push (cons path (org-element-property :begin element)) points))

          (setq element (organki-test--get-element input))
          (push (org-element-property :begin element) points))))
    points))


(defun organki-test--get-element (name)
  "Get a test element by NAME, which is the value of a #+NAME tag line."

  (let ((data (org-element-parse-buffer 'element)))
    (org-element-map data '(plain-list src-block example-block)
      (lambda (element)
        (when (string= name (org-element-property :name element))
          (if (eq (org-element-type element) 'example-block)
              (organki-test--output :points
                                    (org-element-property :begin element))
            element)))
      nil t)))


(defun organki-test--import-region (output)
  "Call `organki/import-region' over an input according to ATTR_ARGS and compare
the results with EXPECTS."

  (let ((attr_args (oref output attr_args))
        (expects (mapcar (lambda (element)
                           (let* ((attr_args2 (oref element attr_args))
                                  (composite (plist-get attr_args2 :composite)))
                             (cons composite (oref element content))))
                         (oref output elements)))
        (point (point))
        results)
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (prompt &optional initial-input history default-value)
                     (cond ((string-match "Notetype" prompt)
                            ;; Here you can reference variables in
                            ;; `organki/import-region', for example,
                            ;; `attr_anki'. If there's a variable also named
                            ;; `attr_args' in `organki/import-region' it would
                            ;; override this one here.
                            (or (plist-get attr_args :notetype) default-value))
                           ((string-match "Deck" prompt)
                            (or (plist-get attr_args :deck) default-value))
                           ((string-match "Tags" prompt)
                            (or (plist-get attr_args :tags) default-value))
                           (t default-value))))
                  ((symbol-function 'read-directory-name)
                   (lambda (prompt &optional dir default-dirname mustmatch initial)
                     default-dirname)))

          (setq results (call-interactively 'organki/import-region))
          (dolist (result results)
            (let* ((result-file (cdr result))
                   (result-buffer (find-file-noselect result-file))
                   (output (with-current-buffer result-buffer
                             (goto-char (point-max))
                             (buffer-substring-no-properties (point-min)
                                                             (point-max))))
                   (expect (or (alist-get (car result) expects)
                               (cdar expects))))
              (organki-test--compare output expect point))))

      ;; Delete the result buffers and files
      (dolist (result results)
        (let* ((result-file (cdr result))
               (result-buffer (find-file-noselect result-file)))
          (with-current-buffer result-buffer
            (save-buffer)
            (should (kill-buffer)))
          (delete-file result-file nil)
          ;; Delete from recent files
          (setq recentf-list (delete (abbreviate-file-name result-file)
                                     recentf-list)))))))


(defun organki-test--compare (output expect &optional point)
  (let ((success (equal output expect))
        diff)
    (when (not success)
      ;; Close the source editing buffer.
      (when (string-prefix-p "*organki-test-input-" (buffer-name))
        (org-edit-src-abort))

      (setq diff (string-diff output expect 10))
      (ert-info ((format (concat "Output doesn't match expect "
                                 "at point %s in %s.\n"
                                 ">> Output:\n%s\n"
                                 ">> Expect:\n%s\n"
                                 ">> Diff: index=%s, diff=%s")
                         point (current-buffer) output expect
                         (car diff) (cadr diff))
                 :prefix "Error: ")
        (should success)))))

(provide 'organki-test-common)
