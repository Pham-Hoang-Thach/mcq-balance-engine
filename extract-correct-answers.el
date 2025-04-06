(require 'subr-x)  ;; For string-trim

(defun extract-correct-answers (file-path)
  "Extract :Correct: answers from the current Org buffer and save to FILE-PATH as a CSV.
   Each output line is in the format: Question,Answer.
   Prompts the user for FILE-PATH to save."
  (interactive
   (list
    (read-file-name "Enter file path to save answers (as CSV): ")))

  (let ((question-number 1)
        (csv-lines '("Question,Answer")))  ;; Initialize with header

    (save-excursion
      (goto-char (point-min))  ;; Start from the beginning of the current buffer
      (while (re-search-forward "^:Correct: \\(.*\\)$" nil t)
        (let ((answer (string-trim (match-string 1))))
          (push (format "%d,%s" question-number answer) csv-lines)
          (setq question-number (1+ question-number)))))

    ;; Write to CSV file in one go
    (with-temp-file file-path
      (insert (mapconcat #'identity (nreverse csv-lines) "\n"))
      (insert "\n"))  ;; Final newline for good measure

    (message "Answers extracted and saved to %s" file-path)))
