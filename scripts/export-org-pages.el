;;; export-org-pages.el --- Split content.org into Jekyll pages -*- lexical-binding: t; -*-

;; Usage:
;;   emacs --batch -l scripts/export-org-pages.el

(require 'subr-x)

(defvar org-pages-source "content.org")

(defun org-pages--yaml-value (value)
  "Return VALUE formatted for simple YAML front matter."
  (cond
   ((member value '("true" "false")) value)
   ((string-match-p "\\`[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\'" value) value)
   ((or (string-match-p "[:{}\\[\\],&*#?|<>=!%@`]" value)
        (string-match-p "\\`\\|\\s-\\'" value))
    (format "%S" value))
   (t value)))

(defun org-pages--split-list (value)
  "Split a comma-separated Org property VALUE."
  (mapcar #'string-trim (split-string value "," t "[[:space:]\n]*")))

(defun org-pages--front-matter (title props)
  "Build YAML front matter for TITLE from PROPS."
  (let ((keys '(("LAYOUT" . "layout")
                ("TITLE" . "title")
                ("PERMALINK" . "permalink")
                ("AUTHOR_PROFILE" . "author_profile")
                ("SITEMAP" . "sitemap")
                ("MODIFIED" . "modified")))
        (list-keys '(("REDIRECT_FROM" . "redirect_from")
                     ("INCLUDE" . "include")))
        (lines (list "---")))
    (dolist (pair keys)
      (let* ((org-key (car pair))
             (yaml-key (cdr pair))
             (value (cdr (assoc org-key props))))
        (when (and (equal org-key "TITLE") (not value))
          (setq value title))
        (when value
          (push (format "%s: %s" yaml-key (org-pages--yaml-value value)) lines))))
    (dolist (pair list-keys)
      (let ((value (cdr (assoc (car pair) props))))
        (when value
          (push (format "%s:" (cdr pair)) lines)
          (dolist (item (org-pages--split-list value))
            (push (format "  - %s" (org-pages--yaml-value item)) lines)))))
    (push "---" lines)
    (concat (string-join (nreverse lines) "\n") "\n\n")))

(defun org-pages--strip-properties (lines)
  "Return (PROPS . BODY-LINES) from page LINES."
  (let ((props nil))
    (when (and lines (string= (string-trim (car lines)) ":PROPERTIES:"))
      (setq lines (cdr lines))
      (while (and lines (not (string= (string-trim (car lines)) ":END:")))
        (when (string-match "^:\\([^:]+\\):[[:space:]]*\\(.*\\)$" (car lines))
          (push (cons (match-string 1 (car lines))
                      (string-trim (match-string 2 (car lines))))
                props))
        (setq lines (cdr lines)))
      (when lines
        (setq lines (cdr lines))))
    (cons (nreverse props) lines)))

(defun org-pages--convert-link-syntax (line)
  "Convert the common Org link forms in LINE to Markdown links."
  (let ((start 0))
    (while (string-match "\\[\\[\\([^]\n]+?\\)\\]\\[\\([^]\n]+?\\)\\]\\]" line start)
      (setq line (replace-match "[\\2](\\1)" t nil line))
      (setq start 0))
    line))

(defun org-pages--render-body (lines)
  "Render page body LINES into Jekyll-friendly Markdown/HTML."
  (let ((out nil)
        (raw nil))
    (dolist (line lines)
      (cond
       ((string-match-p "^#\\+BEGIN_EXPORT\\b" line)
        (setq raw t))
       ((string-match-p "^#\\+END_EXPORT\\b" line)
        (setq raw nil))
       (raw
        (push line out))
       ((string-match "^\\(\\*\\*+\\)[[:space:]]+\\(.*\\)$" line)
        (let ((level (length (match-string 1 line)))
              (heading (org-pages--convert-link-syntax (match-string 2 line))))
          (push (concat (make-string level ?#) " " heading) out)))
       (t
        (push (org-pages--convert-link-syntax line) out))))
    (string-trim-right (string-join (nreverse out) "\n"))))

(defun org-pages--write-page (title props body-lines)
  "Write one page with TITLE, PROPS and BODY-LINES."
  (let ((file (cdr (assoc "EXPORT_FILE_NAME" props))))
    (unless file
      (error "Missing EXPORT_FILE_NAME for page %S" title))
    (make-directory (file-name-directory file) t)
    (let ((mode (when (file-exists-p file)
                  (file-modes file))))
      (with-temp-file file
        (insert (org-pages--front-matter title props))
        (insert (org-pages--render-body body-lines))
        (insert "\n"))
      (when mode
        (set-file-modes file mode)))))

(defun org-pages-export ()
  "Export all first-level headings in `org-pages-source'."
  (let ((current-title nil)
        (current-lines nil)
        (raw-source nil))
    (with-temp-buffer
      (insert-file-contents org-pages-source)
      (dolist (line (split-string (buffer-string) "\n"))
        (cond
         ((and current-title (string-match-p "^#\\+BEGIN_EXPORT\\b" line))
          (push line current-lines)
          (setq raw-source t))
         ((and current-title raw-source)
          (push line current-lines)
          (when (string-match-p "^#\\+END_EXPORT\\b" line)
            (setq raw-source nil)))
         ((string-match "^\\* \\(.*\\)$" line)
            (let ((next-title (match-string 1 line)))
              (when current-title
                (let* ((parsed (org-pages--strip-properties (nreverse current-lines)))
                       (props (car parsed))
                       (body (cdr parsed)))
                  (org-pages--write-page current-title props body)))
              (setq current-title next-title)
              (setq current-lines nil)))
         (current-title
          (push line current-lines))))
      (when current-title
        (let* ((parsed (org-pages--strip-properties (nreverse current-lines)))
               (props (car parsed))
               (body (cdr parsed)))
          (org-pages--write-page current-title props body))))))

(org-pages-export)
