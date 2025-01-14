;; Set directory variables for this project. See info::#Per-Directory Local
;; Variables. Directory variables are actually file variables (buffer local
;; variables) and they're evaluated every time you enter a file or directory of
;; the current project. So set them here only if they're buffer local variables.
;; For global variables set them in a designated configuration file.
((org-mode . (;; There are many external links inside the org files that use
              ;; [[info:org#Link abbreviations][link abbreviations]]. Make sure
              ;; to set these locations in order to generate the correct links.
              (org-link-abbrev-alist . (("d" . "~/archive/document/")
                                        ("p" . "~/projects/"))))))
