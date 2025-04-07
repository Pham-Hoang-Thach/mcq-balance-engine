(require 'cl-lib)

;; Shuffle a list using the Fisher-Yates shuffle algorithm.
(defun shuffle-list (list)
  "Return a new shuffled copy of LIST using Fisher–Yates shuffle."
  (let ((vec (vconcat list)))  ;; Convert the list to a vector for in-place manipulation
    (cl-loop for i from (1- (length vec)) downto 1
             do (cl-rotatef (aref vec i) (aref vec (random (1+ i)))))
    (append vec nil)))  ;; Convert the vector back to a list

;; Generate the file name for saving unique combinations.
(defun unique-combinations-file-for-org (org-file)
  (concat (file-name-directory org-file)
          (file-name-base org-file)
          "-unique-combinations.el"))

;; Load previously saved unique combinations from file.
(defun load-unique-combinations (org-file)
  (let ((data-file (unique-combinations-file-for-org org-file)))
    (if (file-exists-p data-file)
        (with-temp-buffer
          (insert-file-contents data-file)
          (read (current-buffer)))
      '())))  ;; Return empty list if no combinations exist.

;; Save unique combinations to a file.
(defun save-unique-combinations (org-file combinations)
  (let ((data-file (unique-combinations-file-for-org org-file)))
    (with-temp-file data-file
      (prin1 combinations (current-buffer)))))

;; Generate the file name for saving used IDs.
(defun get-used-ids-file (org-file)
  (concat org-file ".used-ids.el"))

;; Load previously used IDs from file.
(defun load-used-ids (org-file)
  (let ((file (get-used-ids-file org-file)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (read (current-buffer)))
      '())))  ;; Return empty list if no IDs exist.

;; Save used IDs to a file.
(defun save-used-ids (org-file ids)
  (let ((file (get-used-ids-file org-file)))
    (with-temp-file file
      (prin1 ids (current-buffer)))))

;; Generate a unique random 3-digit ID (between 100 and 999).
(defun get-unique-random-id (org-file)
  "Generate a unique random 3-digit number (100–999)."
  (let* ((used-ids (load-used-ids org-file))
         (available-ids (cl-set-difference (number-sequence 100 999) used-ids)))
    (if (null available-ids)
        (error "No more unique 3-digit numbers available.")
      (let ((new-id (nth (random (length available-ids)) available-ids)))
        (save-used-ids org-file (cons new-id used-ids))
        new-id))))

;; Normalize a combination of headings for comparison (concatenate them into a string).
(defun normalize-combination (combo)
  "Normalize a heading combination for comparison."
  (mapcar (lambda (heading)
            (mapconcat #'identity heading "\n"))
          combo))

;; Display a summary of available tags and their counts in a new buffer.
(defun display-tag-summary (all-tag-headings)
  "Display a summary of available tags and their counts in a new buffer."
  (let ((buffer (get-buffer-create "*Org Tags Summary*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "* Below is the list of available tags, and the respective counts represent the maximum number a user can input for each tag:\n\n")
      (maphash (lambda (tag headings)
                 (insert (format "** %s: %d available\n" tag (length headings))))
               all-tag-headings)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

;; Main function that combines headings by tags and allows the user to select a unique combination.
(defun persistent-org-combo-shuffler ()
  "Select unique org headings by tag-input with persistent uniqueness."
  (interactive)

  (let* ((org-file (buffer-file-name))
         (all-tag-headings (make-hash-table :test 'equal)))

    ;; Step 1: Index headings by tag
    (save-excursion
      (goto-char (point-min))
      (let ((current-heading nil)
            (current-tags nil))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (cond
             ;; Match a heading line with tags
             ((string-match "^\\*+.*\\(:[a-zA-Z0-9_@:]+:\\)" line)
              (when current-heading
                ;; Add previous heading to tags in hash table
                (dolist (tag current-tags)
                  (puthash tag (cons (reverse current-heading)
                                     (gethash tag all-tag-headings)) all-tag-headings)))
              ;; Extract tags from the current line
              (setq current-tags (split-string (replace-regexp-in-string ":" "" (match-string 1 line)) ":" t))
              ;; Start a new heading
              (setq current-heading (list line)))
             ((and current-heading) (push line current-heading))))  ;; Append lines to current heading
          (forward-line 1))
        ;; Save last heading if there is one
        (when current-heading
          (dolist (tag current-tags)
            (puthash tag (cons (reverse current-heading)
                               (gethash tag all-tag-headings)) all-tag-headings)))))

    ;; Step 2: Display the tags and their available counts
    (display-tag-summary all-tag-headings)

    ;; Step 3: Validate the tag input (check for valid tags and counts)
    (let ((tag-counts '())   ;; Initialize an empty list for parsed tag counts
          (valid-input nil)
          (prompt "Enter tags and numbers (e.g., math=2,science=3): "))
      (while (not valid-input)
        (let* ((tag-input (read-string prompt))
               (parsed-tag-counts '())  ;; Reset parsed counts each time
               (valid t))
          ;; Validate that input is not empty
          (if (not (string-blank-p tag-input))
              (dolist (pair (split-string tag-input "," t))  ;; Split input by commas
                (let* ((split (split-string pair "="))
                       (tag (car split))
                       (count (and (cadr split) (string-to-number (cadr split))))
                       (available (gethash tag all-tag-headings)))
                  ;; Check if the tag exists and if there are enough available headings
                  (if (or (null available) (< (length available) count))
                      (setq valid nil)  ;; Invalid if tag not found or count exceeds available
                    (push (cons tag count) parsed-tag-counts))))
            (setq valid nil))  ;; Invalid if input is empty

          (if valid
              (progn
                (setq tag-counts (nreverse parsed-tag-counts))  ;; Reverse the parsed counts for correct order
                (setq valid-input t))  ;; Valid input, exit the loop
            (setq prompt "Wrong input. Re-Enter tags and numbers (e.g., math=2,science=3): "))))

      ;; Step 4: Ask if the user wants to display tags in the output
      (let* ((show-tags-choice (completing-read "Show tags in output? " '("Yes" "No")))
             (show-tags (string= show-tags-choice "Yes"))
             (previous-combinations (load-unique-combinations org-file))
             final-combination)

        ;; Step 5: Generate a unique combination
        (let ((attempt 0)
              (max-attempts 1000)
              (unique-combo-found nil))
          (while (and (< attempt max-attempts) (not unique-combo-found))
            (setq attempt (1+ attempt))
            (let ((combo '()))
              (dolist (tag-count tag-counts)  ;; Process each tag-count pair
                (let* ((tag (car tag-count))  ;; tag
                       (count (cdr tag-count))  ;; count
                       (available (shuffle-list (copy-sequence (gethash tag all-tag-headings))))
                       (selected '()))
                  ;; Select random headings for the tag
                  (while (and available (< (length selected) count))
                    (let ((candidate (pop available)))
                      (unless (or (member candidate selected)
                                  (seq-some (lambda (combo)
                                              (seq-some (lambda (x) (equal x candidate)) combo))
                                            previous-combinations))
                        (push candidate selected))))
                  ;; Fallback if not enough headings are found
                  (when (< (length selected) count)
                    (let ((fallbacks (shuffle-list (copy-sequence (gethash tag all-tag-headings)))))
                      (while (and fallbacks (< (length selected) count))
                        (let ((extra (pop fallbacks)))
                          (unless (member extra selected)
                            (push extra selected))))))
                  (setq combo (append combo selected))))
              (setq combo (shuffle-list combo))
              ;; Check for uniqueness
              (unless (seq-some (lambda (prev)
                                  (equal (normalize-combination combo)
                                         (normalize-combination prev)))
                                previous-combinations)
                (setq final-combination combo)
                (setq unique-combo-found t))))

        ;; Step 6: Display the result or failure message
        (if (not final-combination)
            (message "Failed to generate unique combo after %d attempts." max-attempts)
          (push final-combination previous-combinations)
          (save-unique-combinations org-file previous-combinations)
          (let ((buffer (get-buffer-create "*Unique Org Headings*"))
                (code (get-unique-random-id org-file)))
            (with-current-buffer buffer
              (erase-buffer)
              (org-mode)
              (insert (format "*Exam code:* %d\n\n" code))
              (dolist (heading final-combination)
                (let ((header-line (car heading)))
                  (insert (if show-tags
                              header-line
                            (replace-regexp-in-string "\\(:[a-zA-Z0-9_@:]+:\\)" "" header-line))
                          "\n")
                  (dolist (line (cdr heading))
                    (cond
                     ((string-match-p "^\\s-*:END:" line)
                      (insert line "\n\n"))
                     ((string-match-p "^\\s-*:PROPERTIES:" line)
                      (insert line "\n"))
                     ((string-match-p "^\\s-*:.*" line)
                      (insert line "\n"))
                     (t
                      (insert line "\n\n"))))
                  (insert "\n"))))
            (switch-to-buffer buffer)
            (message "Unique combination generated in %d attempt(s)." attempt)))))))) 