(defun mcq-export-cleaned-questions ()
  "Reformat Org buffer by renumbering questions as 'Question N' or 'Câu N',
removing properties, preserving '*Exam code:* NNN', and asking whether to show tags.
Adds blank lines after Org-mode tables but not before them."

  (interactive)
  (let ((use-vietnamese (y-or-n-p "Use Vietnamese format ('Câu') for question headings? "))
        (show-tags (y-or-n-p "Show tags in cleaned output? "))
        (output-buffer (get-buffer-create "*Formatted Questions*"))
        (count 1)
        (exam-code-line nil)
        (inside-table nil))  ;; Track if we are inside a table

    ;; Prepare output buffer
    (with-current-buffer output-buffer
      (erase-buffer)
      (org-mode))

    ;; Scan the buffer
    (save-excursion
      (goto-char (point-min))

      ;; Check if exam code line exists
      (when (re-search-forward "^\\*Exam code:\\*\\s-*\\([0-9]+\\)" nil t)
        (setq exam-code-line (buffer-substring-no-properties (match-beginning 0) (match-end 0))))

      ;; Write Exam code line first if found
      (when exam-code-line
        (with-current-buffer output-buffer
          (insert exam-code-line "\n\n")))

      ;; Restart scan from beginning
      (goto-char (point-min))

      ;; Process all headings except Exam code
      (while (re-search-forward "^\\*+\\s-+\\(.*\\)$" nil t)
        (let* ((heading-line (match-string 0))
               (is-exam-code (string-match "^\\*Exam code:\\*" heading-line))
               (start (match-beginning 0))
               (end (save-excursion
                      (if (re-search-forward "^\\*+\\s-" nil t)
                          (match-beginning 0)
                        (point-max)))))

          (unless is-exam-code
            (let ((section (buffer-substring-no-properties start end))
                  (in-properties nil)
                  (lines nil)
                  (tag-part "")
                  (question-text "")
                  (body-lines '()))

              ;; Split and parse lines
              (setq lines (split-string section "\n" t))

              ;; Extract tags from heading
              (when (string-match ":\\([^:\n]+\\(?::[^:\n]+\\)*\\):\\s-*$" (car lines))
                (setq tag-part (match-string 0 (car lines))))

              ;; Skip heading line
              (setq lines (cdr lines))

              ;; Process lines, skipping :PROPERTIES: drawer
              (dolist (line lines)
                (cond
                 ((string-match "^\\s-*:PROPERTIES:" line)
                  (setq in-properties t))
                 ((string-match "^\\s-*:END:" line)
                  (setq in-properties nil))
                 ((not in-properties)
                  (push line body-lines))))

              (setq body-lines (nreverse body-lines))
              (setq question-text (car (seq-drop-while #'string-blank-p body-lines)))
              (setq body-lines (cdr (member question-text body-lines)))

              ;; Insert cleaned heading and content
              (with-current-buffer output-buffer
                (insert (format "*%s %d:* %s" (if use-vietnamese "Câu" "Question") count question-text))
                (when (and show-tags tag-part)
                  (insert "  " tag-part))
                (insert "\n\n")

                ;; Process the body lines
                (dolist (line body-lines)
                  (cond
                   ;; Table handling: Do NOT add a blank line before, but **always** add one after
                   ((string-prefix-p "|" line)
                    (insert line "\n")
                    (setq inside-table t))

                   ;; End of table: **Force a blank line**
                   ((and inside-table (not (string-prefix-p "|" line)))
                    (setq inside-table nil)
                    (insert "\n" line "\n\n"))  ;; **Ensures a blank line after the table**

                   ;; Non-table content (add blank line after regular content)
                   (t
                    (unless (string-blank-p line)
                      (insert line "\n\n"))))))  ;; Blank line after non-table content

            (setq count (1+ count)))))))

    ;; Show result
    (switch-to-buffer output-buffer)))

(provide 'mcq-export-cleaned-questions)