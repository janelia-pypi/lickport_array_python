(require 'org)

(eval-after-load "org"
  '(require 'ox-gfm nil t))

(setq org-confirm-babel-evaluate nil)

(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

(defun tangle-org (org-file)
  "Tangle org file"
  (unless (string= "org" (file-name-extension org-file))
    (error "INFILE must be an org file."))
  (org-babel-tangle-file org-file))

(tangle-org "README.org")

(defun export-org-to-gfm (org-file)
  "Export org file to gfm file"
  (unless (string= "org" (file-name-extension org-file))
    (error "INFILE must be an org file."))
  (let ((org-file-buffer (find-file-noselect org-file)))
    (with-current-buffer org-file-buffer
      (org-open-file (org-gfm-export-to-markdown)))))

(export-org-to-gfm "README.org")
