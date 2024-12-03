(setq doc-dir "~/my-notes")

(setq frame-title-format (list "My-Notes: %b " (if (buffer-file-name) (format "(%s)" (file-name-directory  (buffer-file-name))) "")))

(set-background-color "AntiqueWhite1")
(set-foreground-color "black")

(cd doc-dir)
(dired doc-dir)
