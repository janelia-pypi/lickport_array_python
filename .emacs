(require 'org)

(eval-after-load "org"
  '(require 'ox-gfm nil t))

(setq make-backup-files nil)
(setq enable-local-variables :safe)
(defvar org-doc-author "Peter Polidoro")

(setq org-confirm-babel-evaluate nil)

(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

(defun tangle-org (org-file)
  "Tangle org file"
  (unless (string= "org" (file-name-extension org-file))
    (error "INFILE must be an org file."))
  (org-babel-tangle-file org-file))

(defun export-org (org-file)
  "Export org file to gfm file"
  (unless (string= "org" (file-name-extension org-file))
    (error "INFILE must be an org file."))
  (let ((org-file-buffer (find-file-noselect org-file)))
    (with-current-buffer org-file-buffer
      (org-open-file (org-gfm-export-to-markdown)))))

(defun process-org (org-file)
  "Tangle and export org file"
  (progn (tangle-org org-file)
         (export-org org-file)))

;; https://emacs.stackexchange.com/questions/46020/using-file-local-variables-in-org-mode
(defun my-org-link-eval (path &rest _rest)
  "Evaluate PATH and return result as string."
  (condition-case err
      (prin1-to-string (eval (read path)))
    (error (format "Error in eval of %S: %S." path err))))

(defun my-org-link-eval-activate (start end path bracketp)
  "Display text from START to END as result of the eval of PATH.
BRACKETP is ignored."
  (save-excursion
    (if org-descriptive-links
        (add-text-properties
         start end
         (list 'display (propertize (my-org-link-eval path) 'face 'org-link)))
      (remove-text-properties start end '(display nil)))))

(org-link-set-parameters "val"
                         :export #'my-org-link-eval
                         :activate-func #'my-org-link-eval-activate)
