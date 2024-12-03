(setq work-doc-dir "~/work-doc")
(setq doc-dir (concat work-doc-dir "/doc"))
(setq notes-dir (concat work-doc-dir "/notes"))
(setq cards-dir (concat work-doc-dir "/cards"))


(defun create-notes-buffers()
  (find-file "draft")
  (find-file "worklog")
  )


(setq frame-title-format (list "Work-Doc: %b " (if (buffer-file-name) (format "(%s)" (file-name-directory  (buffer-file-name))) "")))


(set-background-color "cornsilk")
(set-foreground-color "black")

(savehist-mode -1)


(cd notes-dir)
(create-notes-buffers)
(dired cards-dir)
