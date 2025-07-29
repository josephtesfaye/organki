;; -*- lexical-binding: t; -*-
;;; organki.el --- A package that bridges Org Mode and Anki.

;; Copyright (C) 2024 Joseph Huang

;; Author: josephtesfaye
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides functions for dealing with or assisting the usage of Anki the flashcard
;; software.

;;; Code:
(require 'jts-hash-table-utils)
(require 'jts-string-utils)
(require 'jts-buffer-utils)
(require 'jts-org-utils)
(require 'jts-func-utils)

(defconst organki--NS 'Sentence "The \"Sentence\" notetype.")
(defconst organki--NV 'Vocabulary "The \"Vocabulary\" notetype.")
(defconst organki--key-alt "A" "Key for the \"Alternatives\" sub-notes.")
(defconst organki--key-pron "P" "Key for the \"Pronunciation\" sub-notes.")
(defconst organki--key-trans "T" "Key for the \"Translation\" sub-notes.")
(defconst organki--key-comment "C" "Key for the \"Comments\" sub-notes.")
(defconst organki--key-example "E" "Key for the \"Examples\" sub-notes.")
(defconst organki--key-sentence "S" "Key for the \"Sentences\" sub-notes.")
(defconst organki--key-family "F" "Key for the \"Family\" sub-notes.")
(defconst organki--key-link "L" "Key for the \"Links\" sub-notes.")
(defconst organki--key-voc "V" "Key for the \"Vocabulary\" sub-notes.")
(defconst organki--key-mul "M" "Key for the \"Multiple Entry Lines\" sub-notes.")
(defconst organki--SPR-lines :L "Key for Lines in the SPR specs.")
(defconst organki--SPR-groups :G "Key for Groups in the SPR specs.")
(defconst organki--SPR-all :A "Key for All in the SPR specs.")


(defclass organki--anki-vocabulary ()
  ((entry :initarg :entry
          :initform ""
          :type string
          :documentation "The entry of the note.")
   (pronunciation :initarg :pronunciation
                  :initform nil
                  :type (or null string)
                  :documentation "The pronunciation of the entry.")
   (class :initarg :class
          :initform nil
          :type (or null string)
          :documentation "The class of the entry.")
   (translation :initarg :translation
                :initform ""
                :type string
                :documentation "The translation of the entry.")
   (notes :initarg :notes
          :initform nil
          :type (or null hash-table)
          :documentation "The notes about the entry.")
   (parent :initarg :parent
           :initform nil
           :type (or null integer)
           :documentation "The parent item."))

  :documentation "A class representing the Anki notetype named \"Vocabulary\".")


(defclass organki--anki-sentence ()
  ((entry :initarg :entry
          :initform ""
          :type string
          :documentation "The entry of the note.")
   (pronunciation :initarg :pronunciation
                  :initform nil
                  :type (or null string)
                  :documentation "The pronunciation of the entry.")
   (translation :initarg :translation
                :initform nil
                :type (or null string)
                :documentation "The translation of the entry.")
   (notes :initarg :notes
          :initform nil
          :type (or null hash-table)
          :documentation "The notes about the entry.")
   (parent :initarg :parent
           :initform nil
           :type (or null integer)
           :documentation "The parent item."))

  :documentation "A class representing the Anki notetype named \"Sentence\".")


(defvar-local-toggle organki/import-region-open-files nil
  "Whether to open the generated files upon calling `import-region'. If non-nil
the generated files will be opened automatically.")


(defvar-local-toggle organki/import-region-disable-tags nil
  "Whether to disable tags upon calling `import-region'. If it is non-nil there
will be no tags for the generated items. This is useful when you want to update
the existing notes through importing but don't want the existing tags to be
overwritten.")


(defvar-local-toggle organki/import-region-disable-APR nil
  "Whether to disable APR (Automatic Parent Reference) for `import-region'. If
it is non-nil APR will be disabled and the generated children will have no
reference to the main entry.")


(defun organki/import-region (start end &optional output-dir notetype deck tags)
  "Convert a region from START to END, which is supposed to contain a Vocabulary
list or a Sentence list, to a plain-text format for importing to Anki. Only
items completely fall into the region will be selected for the conversion. If
either START or END is `nil' then there is no region specified and the whole
list at the point START or END (whichever is non-`nil') will be selected for the
conversion. If both are `nil' the current point is used.

If the conversion is successful a file named \"import<timestamp>.txt\" will be
generated under OUTPUT-DIR.

Both Vocabulary and Sentence lists have these properties: NOTETYPE, DECK, and
TAGS, which correspond to the same-name concepts in Anki. These provide the
default values for the notes when generating the files for importing to Anki.
You can specify them in heading drawers by the names `anki_notetype',
`anki_deck', and `anki_tags'. You can also specify them directly on a
`\#+ATTR_ANKI' tag line of a list, in a plist such as:

  :notetype \"my_notetype\" :deck \"my_deck\" :tags \"tag1 tag2\"

For notetype and deck the one closest to the list takes precedence if they occur
multiple times in the subree of the list. For tags all occurrences are
accumulated for use along with tags on the headlines of the subtree. The tags
are transformed into a sort of hierarchical tags according to the hierarchy of
the headings, which will then be displayed hierarchically in the tag tree of the
Anki Browser. See `organki/get-default-tags'"

  (interactive
   (let* ((attr-anki (org-list-get-attr-args :attr_anki))
          (default-output-dir "~/Downloads")
          (default-notetype (or (plist-get attr-anki :notetype)
                                (org-entry-get nil "ANKI_NOTETYPE" t)))
          (default-deck (or (plist-get attr-anki :deck)
                            (org-entry-get nil "ANKI_DECK" t)))
          (default-tags (string-join (organki/get-default-tags attr-anki) " "))
          (output-directory (read-directory-name
                             (format "Output directory (default '%s'): " default-output-dir)
                             default-output-dir default-output-dir t))
          (notetype (read-string (format "Notetype (default '%s'): " default-notetype)
                                 nil nil default-notetype))
          (deck (read-string (format "Deck (default '%s'): " default-deck) nil nil default-deck))
          (tags (when (not organki/import-region-disable-tags)
                  (read-string (format "Tags (default '%s'): " default-tags) nil nil default-tags))))

     (when (string-empty-p notetype) (setq notetype nil))
     (when (string-empty-p deck) (setq deck nil))
     (when (string-empty-p tags) (setq tags nil))
     (if (region-active-p)
         (list (region-beginning) (region-end) output-directory notetype deck tags)
       (list nil nil output-directory notetype deck tags))))

  (let ((results (organki--convert-region start end notetype deck tags))
        (notetypes (list organki--NV organki--NS))
        filepaths)
    (dolist (result results)
      (when-let ((content (cdr result))
                 (filename (concat "import" (generate-uuid-with-timestamp) ".txt"))
                 (filepath (expand-file-name filename output-dir)))
        (if organki/import-region-open-files
            (with-current-buffer (find-file-noselect filepath)
              (insert content)
              (save-buffer)
              (display-buffer (current-buffer)))
          (write-region content nil filepath))
        (push (cons (car result) filepath) filepaths)
        (message "File written to %s" filepath)))
    ;; Return the filepaths
    filepaths))


(defun organki--convert-region (&optional start end notetype deck tags)
  "Convert a region from START to END, which is supposed to contain a Vocabulary
or Sentence list, to the plain-text format for importing to Anki. Only items
completely fall into the region will be selected for the conversion. If either
START or END is `nil' it is thought that there is no region specified and the
whole list at the point START or END (whichever is non-nil) will be selected for
the conversion. If both are `nil' the whole list at the current point is
selected. Return an alist of notetype-result pairs."

  (let* ((items (org-list-get-topmost-items-in-region start end))
         (results (if (and notetype (eq organki--NS (intern notetype)))
                      (organki--get-sentences items)
                    (organki--get-vocabulary items)))
         results2)
    ;; Convert the lists to strings
    (dolist (result results)
      (when-let ((key (car result))
                 (val (cdr result)))
        (if (eq key organki--NV)
            (setq val (organki--convert-vocabulary val (symbol-name organki--NV) deck tags))
          (setq val (organki--convert-sentences val (symbol-name organki--NS) deck tags)))
        (push (cons key val) results2)))
    results2))


(defun organki--get-vocabulary (items &optional parent)
  "Convert the REGION which is supposed to contain a Vocabulary list to a list
of `organki--anki-vocabulary' objects."

  (let ((voc-keys (list organki--key-family organki--key-link))
        vocabulary
        sentences)
    (dolist (item items)
      (let ((item-voc-content (organki--get-vocabulary-content item))
            (item-notes (make-hash-table :test 'equal))
            item-voc                    ; The current item converted voc
            child-vocs                  ; The current item's nested vocs
            child-sentences)            ; The current item's nested sentences
        (when-let ((key-items (org-list-item-get-children item)))
          (dolist (key-item key-items)
            (let* ((key-item-children (org-list-item-get-children key-item))
                   (key-item-strs (org-list-item-get-key-item key-item))
                   (key-item-key (car key-item-strs))
                   (key-item-val (cdr key-item-strs))
                   key-item-notes)
              (cond
               ;; Convert Vocabulary sublists
               ((member key-item-key voc-keys)
                (let* ((key-item-results (organki--get-vocabulary key-item-children item))
                       (key-item-vocs (cdr (assoc organki--NV key-item-results)))
                       (key-item-sentences (cdr (assoc organki--NS key-item-results))))
                  (setq key-item-notes (organki--get-direct-children item key-item-vocs))
                  (push key-item-vocs child-vocs)
                  (push key-item-sentences child-sentences)))

               ;; Convert Sentence sublists
               ((equal key-item-key organki--key-sentence)
                (let* ((key-item-results (organki--get-sentences key-item-children item))
                       (key-item-vocs (cdr (assoc organki--NV key-item-results)))
                       (key-item-sentences (cdr (assoc organki--NS key-item-results)))
                       (specs (organki--get-SPR-specs key-item-val)))
                  (setq key-item-notes (organki--get-direct-children item key-item-sentences))
                  ;; Add SPR specs on the key-item line to the current item's
                  ;; child sentences and the "S" sub-notes.
                  (when (plistp specs)
                    (plist-put specs :parent item)
                    (push specs key-item-sentences)
                    (push specs key-item-notes))
                  ;; Create new entries for the "S" sub-notes.
                  (push key-item-vocs child-vocs)
                  (push key-item-sentences child-sentences)))

               ;; Convert sub-items or other types of sublists
               (t (setq key-item-notes (organki--convert-other-notes key-item key-item-strs))))

              (puthash key-item-key key-item-notes item-notes))))

        ;; Construct the current Vocabulary object
        (setq item-voc (organki--anki-vocabulary
                        :entry (organki--convert-fragments (car item-voc-content))
                        :pronunciation (organki--convert-pronunciation
                                        (string-join (seq-subseq item-voc-content 1 -1) " "))
                        :class (nth (- (length item-voc-content) 2) item-voc-content)
                        :translation (car (last item-voc-content))
                        :notes item-notes
                        :parent parent))

        ;; Collect the current voc and its child vocs and sentences (if any) for importing.
        (push item-voc vocabulary)
        (setq child-vocs (apply 'append (nreverse child-vocs)))
        (dolist (voc child-vocs)
          (push voc vocabulary))
        (setq child-sentences (apply 'append (nreverse child-sentences)))
        (push child-sentences sentences)

        ;; Add the current voc to the "V" list of its direct child sentences so
        ;; that it gets displayed in their Notes field.
        (let ((children (organki--get-direct-children item child-sentences)))
          (organki--add-to-notes item-voc children organki--key-voc))
        ;; Add the current voc to the "F" or "L" list of its direct child
        ;; vocabulary items so that it gets displayed in their Notes field.
        (dolist (key voc-keys)
          (organki--add-to-notes item-voc (gethash key item-notes) key))))

    (setq vocabulary (delq nil (nreverse vocabulary)))
    (setq sentences (delq nil (apply 'append (nreverse sentences))))
    (list (cons organki--NS sentences) (cons organki--NV vocabulary))))


(defun organki--get-vocabulary-content (item)
  (let ((content (org-list-item-get-content item t)))
    (setq content (string-split-retain-separators content "\\. " 'after 1))
    (if-let ((part (car content))
             ;; Match English pronunciations, i.e., the part enclosed in `[]'
             ;; which occurs first for all such parts and doesn't contain
             ;; numbers.
             (regexp "\\[[^0-9]+?\\]")
             ((string-match "\\[.+?\\]" part))
             ((string-match-p regexp (match-string 0 part)))
             ;; ((string-match-p regexp part))
             (splits (string-split-retain-separators part regexp nil 1)))
        (setq content (append splits (cdr content)))
      (setq content (append (string-split part " " t) (cdr content))))
    content))


(defun organki--add-to-notes (parent children key)
  "Add PARENT to CHILDREN's notes (a hash-table) under KEY."

  (dolist (child children)
    (when (or (cl-typep child 'organki--anki-sentence)
              (cl-typep child 'organki--anki-vocabulary))
      (let* ((notes (or (oref child notes) (make-hash-table :test 'equal)))
             (subitems (gethash key notes)))
        (push parent subitems)
        (puthash key subitems notes)
        (oset child notes notes)))))


(defun organki--get-sentences (items &optional parent)
  "Convert a region from START to END, which is supposed to contain a Sentence
list, to a list of `organki--anki-sentence' objects, and a list of
`organki--anki-vocabulary' objects, if any. Only items completely fall into the region
will be selected for the conversion. If either START or END is `nil' then there
is no region specified and the whole top-level list at the point START or END
(whichever is non-`nil') will be selected for the conversion. If both are `nil'
the current point is used.

If there's a Vocabulary sublist under an item, convert it to a list of
`organki--anki-vocabulary' objects for importing. Meanwhile convert it to a Vocabulary
sublist for displaying in the sentence's Notes field, and convert the sentence
for displaying in the Vocabulary entries' Notes field.

Return an alist of notetype/list-of-objects pairs."

  (let (sentences vocabulary)
    (dolist (item items)
      ;; Support multiple sentences on one item. If there are multiple entry
      ;; lines combine them all into one sentence object. Otherwise use the
      ;; first one as the main entry object.
      (let* ((key-items (org-list-item-get-children item))
             (item-sentences (organki--get-item-sentences item key-items))
             (select-all (when (length> item-sentences 1)
                           (organki--select-sentences item-sentences
                                                      (list organki--SPR-all t)
                                                      (make-hash-table :test 'equal))))
             (item-sentence (or (caar select-all) (car item-sentences))))
        (push item-sentence sentences)

        (cond
         ((cl-typep item-sentence 'organki--anki-sentence)
          (oset item-sentence parent parent)
          ;; If item has a sublist, it's the note section.
          (when-let ((key-items)
                     (item-notes (make-hash-table :test 'equal)))
            (let (child-sentences       ; The current item's nested sentences
                  child-vocs)           ; The current item's nested vocs
              ;; Convert the sublist to notes.
              (dolist (key-item key-items)
                (let* ((key-item-children (org-list-item-get-children key-item))
                       (key-item-strs (org-list-item-get-key-item key-item))
                       (key-item-key (car key-item-strs))
                       key-item-notes)
                  (pcase key-item-key
                    ;; If there is a Vocabulary sublist (V:) process it recursively.
                    ((pred (equal organki--key-voc))
                     (let* ((key-item-results (organki--get-vocabulary key-item-children item))
                            (key-item-vocs (cdr (assoc organki--NV key-item-results)))
                            (key-item-sentences (cdr (assoc organki--NS key-item-results))))
                       (push key-item-vocs child-vocs)
                       (push key-item-sentences child-sentences)
                       (setq key-item-notes (organki--get-direct-children item key-item-vocs))))

                    ;; Read the string after the key as the value.
                    (_ (setq key-item-notes (organki--convert-other-notes key-item key-item-strs))))

                  (puthash key-item-key key-item-notes item-notes)))

              ;; Set other properties of the current sentence object.
              (when-let ((pron (gethash organki--key-pron item-notes)))
                (oset item-sentence pronunciation (organki--convert-pronunciation pron)))
              (when-let ((trans (gethash organki--key-trans item-notes)))
                (oset item-sentence translation trans))
              (oset item-sentence notes item-notes)

              ;; Collect the child sentences and vocs (if any).
              (setq child-sentences (apply 'append (nreverse child-sentences)))
              (setq child-vocs (apply 'append (nreverse child-vocs)))
              (push child-vocs vocabulary)

              ;; Use the current sentence or selected sentences as examples in
              ;; the notes of each direct child Vocabulary item.
              (let ((children (organki--get-direct-children item child-vocs))
                    ;; Keep track of the order of generated entries
                    (specs-table (make-hash-table :test 'equal))
                    ;; Keep track of selected entries to prevent duplicate entries
                    (selects-table (make-hash-table :test 'equal)))
                (dolist (voc children)
                  (let* ((notes (or (oref voc notes) (make-hash-table :test 'equal)))
                         (notes-sens1 (gethash organki--key-sentence notes))
                         ;; When there isn't any SPR specs use APR
                         (notes-sens2 (when (and (not organki/import-region-disable-APR)
                                                 (not (seq-find #'plistp notes-sens1)))
                                        (list (or (caadr select-all) item-sentence)))))
                    (dolist (sentence notes-sens1)
                      ;; If sentence is a plist it's the SPR specs. Replace it with
                      ;; the specified entry line objects.
                      (if (plistp sentence)
                          (let* ((selects (organki--select-sentences
                                           item-sentences sentence selects-table))
                                 (sel-ents (car selects))
                                 (sel-sens (cadr selects)))
                            (organki--add-to-notes voc sel-ents organki--key-voc)
                            (puthash sentence sel-ents specs-table)
                            (dolist (sens sel-sens)
                              (push sens notes-sens2)))
                        (push sentence notes-sens2)))
                    (when notes-sens2
                      (setq notes-sens2 (nreverse notes-sens2))
                      (puthash organki--key-sentence notes-sens2 notes)
                      (oset voc notes notes))))

                ;; Reverse the order of the vocs in notes
                (dolist (sentence (hash-table-values selects-table))
                  (let* ((notes (oref sentence notes))
                         (vocs (nreverse (gethash organki--key-voc notes))))
                    (puthash organki--key-voc vocs notes)))

                ;; Collect child sentences for importing and replace the specs
                ;; with the corresponding sentence objects.
                (dolist (sentence child-sentences)
                  (if-let (((plistp sentence))
                           (sel-ents (gethash sentence specs-table)))
                      (dolist (select sel-ents)
                        (cl-pushnew select sentences))
                    (push sentence sentences)))))))

         ;; If it's a SPR specs set the parent for it.
         ((plistp item-sentence)
          (plist-put item-sentence :parent parent)))))

    (setq sentences (delq nil (nreverse sentences)))
    (setq vocabulary (delq nil (apply 'append (nreverse vocabulary))))
    (list (cons organki--NS sentences) (cons organki--NV vocabulary))))


(defun organki--get-item-sentences (item key-items)
  "Convert the content of ITEM (buffer position only) to a list of
`organki--anki-sentence' objects, or convert the `M' sublist if it exists."

  (let* ((mul-key-item (when key-items
                         (seq-find
                          (lambda (key-item)
                            (let* ((key-item-strs (org-list-item-get-key-item key-item))
                                   (key-item-key (car key-item-strs)))
                              (equal key-item-key organki--key-mul)))
                          key-items)))
         (item-lines (if mul-key-item
                         (mapcar #'org-list-item-get-content
                                 (org-list-item-get-children mul-key-item))
                       (split-string (org-list-item-get-content item t) "\n" t " +")))
         item-sentences)

    (dolist (line item-lines)
      (if-let ((specs (organki--get-SPR-specs line))
               ((plistp specs)))
          ;; Add Selective Parent Reference specs
          (push specs item-sentences)

        ;; Create a sentence for each line
        (let* ((split-line (mapcar #'string-trim (string-split-retain-separators
                                                  line "[]:.?。？] " 'after)))
               (audio (when (string-match "⏯" (car split-line))
                        (concat (organki--convert-pronunciation (car split-line)) " ")))
               (speaker-peek (if audio (cadr split-line) (car split-line)))
               (speaker (when (string-match ":" speaker-peek) speaker-peek))
               (contents (cond ((xor audio speaker) (cdr split-line))
                               ((and audio speaker) (cddr split-line))
                               (t split-line)))
               (entry (string-join-non-blanks (list audio speaker (car contents)) " "))
               (translation (string-join-non-blanks
                             (flatten-list (list speaker (cdr contents))) " "))
               (sentence (organki--anki-sentence
                          :entry entry
                          :translation translation)))
          (push sentence item-sentences))))
    (nreverse item-sentences)))


(defun organki--get-SPR-specs (string)
  "Return a plist out of STRING if it's a valid SPR (Selective Parent Reference)
specs. Otherwise, return STRING."

  (if (string-match ":[LGA] " string)
      (organki--read (concat "(" string ")"))
    string))


(defun organki--read (&optional stream)
  "When `debug-on-error' is enabled and the `read' command throws some errors
the stacktrace may not show up, obscuring the bugs. Try the two forms to see
the difference:

  (read \"(0 1\") ; This throws `end-of-file' but stacktrace dosen't show up
  (/ 1 0)       ; This throws `arith-error' and stacktrace shows up
  (signal 'arith-error '(\"error\"))
  (signal 'end-of-file '(\"error\"))

This is because Emacs uses the `debug-ignored-errors' variable to specify
patterns of error messages that should not invoke the debugger, and
`end-of-file' is one of them.

Thus it becomes necessary to write a wrapper around it and signal the error
explicitly."

  (condition-case err
      (read stream)
    (error
     (signal 'error (list 'read stream (error-message-string err))))))


(defun organki--select-sentences (sentences specs selects-table)
  "Select a list of sentences from SENTENCES according to SPECS, which is a plist
containing the :L (short for lines) and :G (short for groups) keywords and their
corresponding lists of indicies (one-based). SELECTS-TABLE is used to cache the
generated entries to prevent duplicates."

  (let* ((sel-ents (copy-sequence specs)) ; Selected entries
         sel-sens                         ; Selected sentences, used for notes
         lines                            ; All single-line entries
         groups                           ; All multi-line entries
         all)                             ; All lines as one entry

    ;; Get lines
    (dolist (index (plist-get specs organki--SPR-lines))
      (let ((final-entry (gethash index selects-table)))
        (when (not final-entry)
          (setq final-entry (elt sentences (1- index)))
          (puthash index final-entry selects-table))
        (push final-entry lines)
        (push final-entry sel-sens)))
    (setq lines (nreverse lines))

    ;; Get groups
    (dolist (group (plist-get specs organki--SPR-groups))
      ;; Get final entry
      (let ((final-entry (gethash group selects-table)))
        (when (not final-entry)
          (let (entries translations)
            (dolist (index group)
              (let ((sen (elt sentences (1- index))))
                (push (oref sen entry) entries)
                (push (oref sen translation) translations)))
            (setq entries (string-join (nreverse entries) "<br>"))
            (setq translations (string-join (nreverse translations) "<br>"))
            (setq final-entry (organki--anki-sentence
                               :entry entries
                               :translation translations))
            (puthash group final-entry selects-table)))
        (push final-entry groups))

      ;; Get sentences for notes
      (push (mapcar (lambda (index)
                      (elt sentences (1- index)))
                    group)
            sel-sens))
    (setq groups (nreverse groups))

    ;; Get all
    (when (plist-get specs organki--SPR-all)
      (let ((final-entry (gethash organki--SPR-all selects-table)))
        (when (not final-entry)
          (let (entries translations)
            (dolist (sen sentences)
              (push (oref sen entry) entries)
              (push (oref sen translation) translations))
            (setq entries (string-join (nreverse entries) "<br>"))
            (setq translations (string-join (nreverse translations) "<br>"))
            (setq final-entry (organki--anki-sentence
                               :entry entries
                               :translation translations))
            (puthash organki--SPR-all final-entry selects-table)))
        (push final-entry all)
        (push sentences sel-sens)))

    ;; Make sure the order of the combined objects corresponds to that of SPECS.
    (when lines (plist-put sel-ents organki--SPR-lines lines))
    (when groups (plist-put sel-ents organki--SPR-groups groups))
    (when all (plist-put sel-ents organki--SPR-all all))
    (setq sel-ents (apply #'append (seq-filter #'listp sel-ents)))
    (setq sel-sens (nreverse sel-sens))
    ;; Return two lists
    (list sel-ents sel-sens)))


(defun organki--get-direct-children (item items)
  "Return the direct children of ITEM (position of an list item) from ITEMS (a
Vocabulary or Sentence list)."

  (seq-filter (lambda (obj)
                (cond
                 ((or (cl-typep obj 'organki--anki-sentence)
                      (cl-typep obj 'organki--anki-vocabulary))
                  (equal (oref obj parent) item))

                 ((plistp obj)
                  (equal (plist-get obj :parent) item))))
              items))


(defun organki--convert-pronunciation (string)
  "Convert the audio file portion of the string."

  (string-replace-fences string "[[cl:" "][⏯]]" "[sound:" "]"))


(defun organki--convert-fragments (string)
  "Format fragments such as furigana, latex, etc. used in the org mode notes to
the form for displaying in Anki."

  ;; Convert furigana
  (when (string-match "\\^{.*}" string)
    (setq string (concat "<ruby>"
                         (string-replace-fences string "^{" "}" "<rt>" "</rt>")
                         "</ruby>")))
  string)


(defun organki--convert-other-notes (key-item key-item-strs)
  "Convert other types of KEY-ITEM to notes. The KEY-ITEM's value and its
sublist (if any) are concatenated into a plain string with format suitable for
displaying in an Anki card."

  (let* ((key-item-content (cdr key-item-strs))
         (contents (when (not (string-blank-p key-item-content))
                     (list (organki--convert-fragments key-item-content))))
         (key-item-subtree (org-list-item-get-subtree key-item))
         (struct (org-list-get-struct key-item))
         ;; Find the minmium indentation
         (min-ind (when key-item-subtree
                    (apply 'min (mapcar (lambda (item)
                                          (org-list-get-ind item struct))
                                        key-item-subtree)))))

    (dolist (item key-item-subtree)
      (when-let ((content (org-list-item-get-content item t))
                 (ind (- (org-list-get-ind item struct) min-ind))
                 ((not (string-blank-p content))))
        (push (concat (string-repeat "&nbsp;" ind)
                      "• " (organki--convert-fragments content))
              contents)))

    (when contents
      (setq contents (nreverse contents))
      (setq contents (string-join (delq nil contents) "<br>")))
    contents))


(defun organki--convert-vocabulary (vocabulary &optional notetype deck tags)
  "Convert a list of `organki--anki-vocabulary' objects to plain-text format."

  (let* ((lines (mapcar (lambda (voc)
                          (let ((temp (string-join
                                       (list (oref voc entry)
                                             (oref voc pronunciation)
                                             (oref voc translation)
                                             (organki--convert-notes (oref voc notes)))
                                       "\t")))
                            (delq nil (list notetype deck tags temp))))
                        vocabulary))
         (result (concat (organki--get-file-headers notetype deck tags)
                         (string-join (mapcar (lambda (line)
                                                (string-join line "\t"))
                                              lines)
                                      "\n"))))
    result))


(defun organki--convert-sentences (sentences &optional notetype deck tags)
  "Convert a list of `organki--anki-sentence' objects to plain-text format."

  (let* ((lines (mapcar (lambda (sentence)
                          (setq sentence (string-join
                                          (list (oref sentence entry)
                                                (oref sentence pronunciation)
                                                (oref sentence translation)
                                                (organki--convert-notes (oref sentence notes)))
                                          "\t"))
                          (string-join (delq nil (list notetype deck tags sentence))
                                       "\t"))
                        sentences))
         (result (concat (organki--get-file-headers notetype deck tags)
                         (string-join lines "\n"))))
    result))


(defun organki--convert-notes (notes)
  "Convert the notes for displaying in Anki cards."

  (when-let (((hash-table-p notes))
             (notes2 (make-hash-table :test 'equal)))
    ;; Convert each type of notes to plain string.
    (maphash (lambda (key val)
               (let ((val2 (cond
                            ((or (equal key organki--key-family)
                                 (equal key organki--key-link)
                                 (equal key organki--key-voc))
                             (concat (organki--convert-notes-subnote-name key)
                                     (organki--convert-notes-vocabulary val)))

                            ((equal key organki--key-sentence)
                             (organki--convert-notes-sentences val))

                            ((equal key organki--key-example)
                             (concat "<p>" val "</p>"))

                            (t (concat (organki--convert-notes-subnote-name key)
                                       "<p>" val "</p>")))))
                 (puthash key val2 notes2)))
             notes)

    ;; Combine contents under both key "E" and "S" for displaying as examples.
    (let ((examples (gethash organki--key-example notes2))
          (sentences (gethash organki--key-sentence notes2)))
      (when (or examples sentences)
        (puthash organki--key-example
                 (concat (organki--convert-notes-subnote-name organki--key-example)
                         (string-join (delq nil (list examples sentences)) "<br>"))
                 notes2)))

    (concat (gethash organki--key-alt notes2)
            (gethash organki--key-comment notes2)
            (gethash organki--key-example notes2)
            (gethash organki--key-voc notes2)
            (gethash organki--key-family notes2)
            (gethash organki--key-link notes2))))


(defun organki--convert-notes-subnote-name (key)
  (pcase key
    ((pred (equal organki--key-alt)) "<b>Alternatives</b>")
    ((pred (equal organki--key-comment)) "<b>Comments</b>")
    ((pred (equal organki--key-example)) "<b>Examples</b>")
    ((pred (equal organki--key-family)) "<b>Family</b>")
    ((pred (equal organki--key-link)) "<b>Linking</b>")
    ((pred (equal organki--key-voc)) "<b>Vocabulary</b>")))


(defun organki--convert-notes-vocabulary (vocabulary)
  "Convert the vocabulary objects in notes to HTML format."

  (let (lines)
    (dolist (voc vocabulary)
      (let ((line (string-join (list (oref voc entry)
                                     (oref voc pronunciation)
                                     (oref voc translation))
                               " ")))
        (setq line (concat "<li>" line "</li>"))
        (push line lines)))
    (when lines
      (setq lines (concat "<ul>" (string-join (nreverse lines)) "</ul>")))
    lines))


(defun organki--convert-notes-sentences (sentences)
  "Convert the sentence objects in notes to HTML format."

  (let (lines)
    (dolist (sentence sentences)
      (let (line)
        (if (cl-typep sentence organki--anki-sentence)
            (setq line (concat (oref sentence entry) " " (oref sentence translation)))
          (setq line (mapcar (lambda (sen)
                               (concat (oref sen entry) " "
                                       (string-after (oref sen translation) ": ")))
                             sentence))
          (setq line (string-join line "<br>")))
        (setq line (concat "<li>" line "</li>"))
        (push line lines)))
    (when lines
      (setq lines (concat "<ul>" (string-join (nreverse lines)) "</ul>")))
    lines))


(defun organki--get-file-headers (notetype deck tags)
  (concat "#separator:tab\n#html:true\n"
          (when notetype "#notetype column:1\n")
          (when deck "#deck column:2\n")
          (when tags "#tags column:3\n")))


(defun organki/get-default-tags (&optional attr-anki)
  "Get the default tags for the current region. If the region is in org mode
retrieve all the tags in the current subtree, i.e., including the tags of the
parent headings, and the tags specified in ATTR-ARGS, in a format conforming
with Anki's tag trees.

For example, for a subtree like the following:

#+NAME: example1-input
#+begin_example
\\* Duolingo                                                         :Duolingo:
\\** Section 1                                                      :Section01:
\\*** Unit 1 Order food, describe people                               :Unit01:
\\**** Vocabulary
- 水 みず n. water
- 寿司 すし n. Sushi
- ご飯 ごはん n. rice; meal
#+end_example

The output tags would be:

#+NAME: example1-output
#+begin_example
Duolingo::Section01::Unit01
#+end_example"

  (let* ((element (org-element-at-point))
         (direct-headline (org-element-lineage element '(headline) t))
         (headline direct-headline)
         (parent nil)
         (is-headline direct-headline)
         (headline-tags nil)
         (tags nil)
         (all-tags '())
         result)
    (while is-headline
      (setq headline-tags (mapcar 'substring-no-properties
                                  (org-element-property :tags headline)))
      (setq tags (org-entry-get (org-element-property :begin headline) "ANKI_TAGS"))
      (when (and tags (not (string-blank-p tags)))
        (setq headline-tags (append headline-tags (string-split tags))))
      (when headline-tags
        (push headline-tags all-tags))
      (setq parent (org-element-property :parent headline))
      (setq is-headline (equal 'headline (org-element-type parent)))
      (when is-headline
        (setq headline parent)))

    (when (plist-get attr-anki :tags)
      (setq all-tags (append all-tags (list (string-split (plist-get attr-anki :tags) " ")))))
    (setq result (organki--combine-lists-to-strings all-tags))
    result))


(defun organki--combine-lists-to-strings (lists)
  "Generate all combinations of picking one element from each sublist in LISTS,
combined as strings."

  (mapcar (lambda (comb)
            (mapconcat 'identity comb "::"))
          (organki--combine-lists lists)))


(defun organki--combine-lists (lists)
  "Generate all combinations of picking one element from each sublist in LISTS.
Hint: This is a problem related to Cartesian Product."

  (if (null lists)
      ;; Base case: no lists to process, return a list with an empty list
      '(())
    ;; Recursive case: process the first list and combine with the result of
    ;; processing the rest
    (let ((first-list (car lists))
          (rest-combinations (organki--combine-lists (cdr lists))))
      (apply 'append
             (mapcar (lambda (item)
                       (mapcar (lambda (comb)
                                 (cons item comb))
                               rest-combinations))
                     first-list)))))


;; * Formatting
(defun organki/format-region-pretty (&optional start end)
  "Replace the Sentence list in the region from START to END with the prettified
list, where the translations and dialogues are aligned more nicely to give more
visual clarity."

  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))

  (let* ((struct (org-list-get-struct (or start end)))
         (items (org-list-get-items-in-region start end))
         (items-start (car items))
         (items-end (org-list-get-item-end (car (last items)) struct))
         (result (organki--sentences-prettified-string start end)))
    (delete-region items-start items-end)
    (goto-char items-start)
    (insert result)))


(defun organki--sentences-prettified-string (&optional start end)
  "Prettify a Sentence list in the region from START to END and return the
prettified result as a string."

  (let* ((table (organki--sentences-get-struct start end))
         (audio-list (gethash 'audio-list table))
         (entry-list (gethash 'entry-list table))
         (translation-list (gethash 'translation-list table))
         (has-audio (seq-find #'identity audio-list))
         (max-width (apply 'max (mapcar #'string-width entry-list)))
         line
         padding
         result)
    (dotimes (i (length entry-list))
      (setq entry (nth i entry-list))
      (setq line (concat (nth i audio-list) entry))
      (when (and has-audio (not (nth i audio-list)))
        (setq line (concat "   " line)))
      (setq padding (when (> max-width (string-width entry))
                      (format (format "%%%ds" (- max-width (string-width entry)))
                              " ")))
      (setq line (concat "- " line padding (nth i translation-list)))
      (push line result))
    (setq result (string-join (nreverse result) "\n"))
    result))


(defun organki--sentences-get-struct (&optional start end)
  "Return the elements of a Sentence list as a hash table where the
elements are grouped into different lists based on their type in a Sentence
item."

  (let ((items (org-list-get-items-in-region start end))
        (table (make-hash-table :test 'equal)))
    (dolist (item items)
      (let* ((org-item (org-list-get-item item))
             (entry-lines (org-list-item-get-content item t))
             (line-start (org-element-property :contents-begin org-item))
             (bullet-len (- line-start (org-element-property :begin org-item)))
             (bullet-spaces (format " \\{%d\\}" bullet-len))
             entry-start line-parts audio entry translation)
        ;; An item may have one or multiple lines
        (setq entry-lines (string-split entry-lines "\n"))
        (dolist (entry-line entry-lines)
          (setq entry-line (string-trim-left entry-line bullet-spaces))
          (setq line-parts (string-split-retain-separators
                            entry-line "[].?。？] +" 'after))
          (setq audio (and (string-match "⏯" (car line-parts)) (car line-parts)))
          (setq entry (or (and audio (cadr line-parts)) (car line-parts)))
          ;; Remove the equal-length leading spaces as the bullet.
          ;; (when audio
          ;;   (setq audio (string-trim-left audio bullet-spaces)))
          ;; (setq entry (string-trim-left entry bullet-spaces))
          ;; Calculate the start position of the entry.
          (setq entry-start (+ line-start (length audio)))
          (setq translation (or (and audio (caddr line-parts)) (cadr line-parts)))
          (hash-table-group table
                            'audio-list audio
                            'entry-list entry
                            'entry-starts entry-start
                            'translation-list translation)
          ;; The next entry line starts from the bullet end.
          (setq line-start (+ line-start (length entry-line) 1 bullet-len)))))

    (puthash 'audio-list (nreverse (gethash 'audio-list table)) table)
    (puthash 'entry-list (nreverse (gethash 'entry-list table)) table)
    (puthash 'entry-starts (nreverse (gethash 'entry-starts table)) table)
    (puthash 'translation-list (nreverse (gethash 'translation-list table)) table)
    table))


(defun organki/prettify-region (&optional start end arg)
  "Prettify a Sentence list in the region from START to END with the same effect
as `organki/format-region-pretty' but don't change anything of the original
content. If ARG is non-nil revert to the original."

  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (list nil nil current-prefix-arg)))
  (if arg
      (organki--unprettify-sentences start end)
    (organki--prettify-sentences start end)))


(defun organki--prettify-sentences (&optional start end)
  "Prettify a Sentence list in the region from START to END with the same effect
as `organki/format-region-pretty' but don't change anything of the original
content."

  (let* ((table (organki--sentences-get-struct start end))
         (audio-list (gethash 'audio-list table))
         (entry-list (gethash 'entry-list table))
         (entry-starts (gethash 'entry-starts table))
         (has-audio (seq-find #'identity audio-list))
         (max-width (apply 'max (mapcar #'string-width entry-list)))
         entry-start
         padding
         overlay)
    (dotimes (i (length entry-list))
      (setq entry (nth i entry-list))
      (setq entry-start (nth i entry-starts))
      (setq overlay (make-overlay entry-start (+ entry-start (1- (length entry)))))
      (overlay-put overlay 'class 'organki)
      (when (and has-audio (not (nth i audio-list)))
        (overlay-put overlay 'before-string "   "))
      (setq padding (when (> max-width (string-width entry))
                      (format (format "%%%ds" (- max-width (string-width entry)))
                              " ")))
      (overlay-put overlay 'after-string padding)
      (overlay-put overlay 'evaporate t))))


(defun organki--unprettify-sentences (&optional start end)
  "Revert a prettified Sentence list in the region from START to END to the
original."

  (let* ((struct (org-list-get-struct (or start end)))
         (items (org-list-get-items-in-region start end))
         (items-start (car items))
         (items-end (org-list-get-item-end (car (last items)) struct))
         (overlays (seq-filter (lambda (ov)
                                 (eq (plist-get (overlay-properties ov) 'class)
                                     'organki))
                               (overlays-in items-start items-end))))
    (mapc #'delete-overlay overlays)))


;; ==== Key-bindings ====
;; Add these commands to org mode key-bindings
(with-eval-after-load 'org
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "i" 'organki/import-region
    "TAB" 'organki/prettify-region))

(provide 'organki)
