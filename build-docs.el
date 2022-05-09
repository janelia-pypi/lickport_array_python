(require 'org)

(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; Don't ask when evaluating code blocks
(setq org-confirm-babel-evaluate nil)

;; (defun export-file-md (org-file md-file)
;;   "Export ORG-FILE to MD-FILE."
;;   (interactive "f\nf")
;;   (unless (string= "org" (file-name-extension org-file))
;;     (error "INFILE must be an org file."))
;;   (unless (string= "md" (file-name-extension md-file))
;;     (error "OUTFILE must have an .md extension."))
;;   (let ((current-buffers (buffer-list))
;;     (tmp-buffer) 
;;     (open (find-buffer-visiting org-file))
;;     (org-file-buffer (find-file-noselect org-file)))
;;     (with-current-buffer org-file-buffer
;;       (with-current-buffer (setq tmp-buffer (org-org-export-as-org))
;;     (insert (format "#+EXPORT_FILE_NAME: %s\n" md-file))
;;     (org-open-file (org-md-export-to-markdown))))
;;     (kill-buffer tmp-buffer)
;;     (unless open (kill-buffer org-file-buffer))))

(defun export-org-readme (org-file)
  "Export ORG-FILE README"
  (unless (string= "org" (file-name-extension org-file))
    (error "INFILE must be an org file."))
  (let ((org-file-buffer (find-file-noselect org-file)))
    (with-current-buffer org-file-buffer
      (org-open-file (org-md-export-to-markdown)))))

(export-org-readme "README.org")
