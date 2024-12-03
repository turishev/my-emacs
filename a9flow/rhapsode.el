;;(require 'a9flow-mode)


;;https://git.area9lyceum.com/Lyceum/lyceum/src/branch/jenkins_rc_910/rhapsode/curator/curator.flow#L25
;;https://git.area9lyceum.com/Lyceum/lyceum/src/branch/master/rhapsode/curator/curator.flow

(defun get-gitea-link()
  (let* (
	 (fname (buffer-file-name (current-buffer)))
	 (branch (magit-get-current-branch))
	 (path-inx (progn (string-match "/lyceum"  fname) 
			  (match-end 0)))
	 (fname-1 (substring fname path-inx))
	 (line-str (concat "#L" (number-to-string (line-number-at-pos))))
	 (link (concat "https://git.area9lyceum.com/Lyceum/lyceum/src/branch/" branch fname-1 line-str))
	 )

    (message "GITEA:%s" link)
    link))


(defun goto-gitea ()
  (interactive)
  (browse-url (get-gitea-link)))

(defun flow-mode-hk ()
  (local-set-key (kbd "<f8>g") 'goto-gitea)
)

(add-hook 'a9flow-mode-hook 'flow-mode-hk)

(compile-target-open-project (concat (getenv "AREA9") "/lyceum/rhapsode/emacs-project.el"))




