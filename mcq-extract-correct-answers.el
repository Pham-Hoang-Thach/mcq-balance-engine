(require 'subr-x)  ;; For string-trim

(defun mcq-extract-correct-answers (file-path)
  "Extract :Correct: answers from the current Org buffer and save to FILE-PATH as a CSV.
Keeps only the most recent answers by deleting everything above the last 'Question,Answer'."
  (interactive
   (list (read-file-name "Enter file path to save answers (as CSV): ")))

  ;; Collect answers from Org buffer
  (let ((answers '("Question,Answer"))
        (qnum 1))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*:Correct: \\(.*\\)$" nil t)
        (push (format "%d,%s" qnum (string-trim (match-string 1))) answers)
        (setq qnum (1+ qnum))))

    ;; Write to temp file
    (let* ((temp-file (make-temp-file "org-answers-" nil ".csv"))
           (clean-content ""))
      (with-temp-file temp-file
        (insert (mapconcat #'identity (nreverse answers) "\n"))
        (insert "\n"))

      ;; Keep only from last 'Question,Answer' onward
      (with-temp-buffer
        (insert-file-contents temp-file)
        (goto-char (point-max))
        (when (re-search-backward "^Question,Answer$" nil t)
          (setq clean-content (buffer-substring-no-properties (point) (point-max)))))

      ;; Write cleaned content to final file
      (with-temp-file file-path
        (insert clean-content)))

    (message "Saved %d answers to %s" (1- qnum) file-path)))

(provide 'mcq-extract-correct-answers)
