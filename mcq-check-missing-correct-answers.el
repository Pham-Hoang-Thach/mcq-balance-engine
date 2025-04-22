(defun mcq-check-missing-correct-answers ()
  "Find questions with a blank :Correct: property and show clickable links to fix them."
  (interactive)
  (let ((results '()))
    (org-map-entries
     (lambda ()
       (let ((correct (org-entry-get (point) "Correct")))
         (when (and correct (string-blank-p correct))
           (let ((link (org-store-link nil)))
             (push link results)))))
     nil 'file)
    (if results
        (let ((buf (get-buffer-create "*Fix Blank :Correct:*")))
          (with-current-buffer buf
            (erase-buffer)
            (insert "* Questions missing :Correct: value\n\n")
            (dolist (link (reverse results))
              (insert "- " link "\n"))
            (org-mode))
          (display-buffer buf))
      (message "All questions have valid :Correct: answers."))))
