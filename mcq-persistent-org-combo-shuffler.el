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
(defun mcq-persistent-org-combo-shuffler ()
  "Select unique org headings by tag-input with persistent uniqueness."
  (interactive)

  (let* ((org-file (buffer-file-name))
         (all-tag-headings (make-hash-table :test 'equal))
         (previous-combinations (load-unique-combinations org-file))
         final-combination)

;; Step 1: Index tags only from top-level parents or leaf nodes
(save-excursion
  (goto-char (point-min))
  (let ((headings '()))
    ;; Collect all headings
    (org-map-entries
     (lambda ()
       (let* ((tags (org-get-tags))
              (level (org-current-level))
              (begin (point))
              (end (save-excursion (org-end-of-subtree t t)))
              (has-children (save-excursion
                              (goto-char begin)
                              (re-search-forward (format "^\\*\\{1,%d\\} " (1+ level)) end t)))
              (content (split-string
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         end)
                        "\n")))
         (when tags
           (push (list :tags tags
                       :level level
                       :has-children has-children
                       :content content)
                 headings))))
     nil 'file)

    ;; Build tag index (filter only top-parents or leafs)
    (let ((filtered-headings
           (seq-filter (lambda (h)
                         (let ((lvl (plist-get h :level))
                               (kids (plist-get h :has-children)))
                           (or (and (= lvl 1) kids)  ; top parent with children
                               (not kids))))        ; leaf node
                       headings)))
      (dolist (h filtered-headings)
        (dolist (tag (plist-get h :tags))
          (puthash tag
                   (cons (plist-get h :content)
                         (gethash tag all-tag-headings))
                   all-tag-headings))))))
    
    ;; Step 2: Display tag summary
    (display-tag-summary all-tag-headings)

    ;; Step 3: Tag input validation
    (let ((tag-counts '())
          (valid-input nil)
          (prompt "Enter tags and numbers (e.g., math=2,science=3): "))
      (while (not valid-input)
        (let* ((tag-input (read-string prompt))
               (parsed-tag-counts '())
               (valid t))
          (if (not (string-blank-p tag-input))
              (dolist (pair (split-string tag-input "," t))
                (let* ((split (split-string pair "="))
                       (tag (car split))
                       (count (and (cadr split) (string-to-number (cadr split))))
                       (available (gethash tag all-tag-headings)))
                  (if (or (null available) (<= count 0) (< (length available) count))
                      (setq valid nil)
                    (push (cons tag count) parsed-tag-counts))))
            (setq valid nil))
          (if valid
              (setq tag-counts (nreverse parsed-tag-counts)
                    valid-input t)
            (setq prompt "Wrong input. Re-Enter tags and numbers (e.g., math=2,science=3): "))))

      ;; Step 4: Generate unique combination
      (let ((attempt 0)
            (max-attempts 1000)
            (unique-combo-found nil))
        (while (and (< attempt max-attempts) (not unique-combo-found))
          (setq attempt (1+ attempt))
          (let ((combo '()))
            (dolist (tag-count tag-counts)
              (let* ((tag (car tag-count))
                     (count (cdr tag-count))
                     (available (shuffle-list (copy-sequence (gethash tag all-tag-headings))))
                     (selected '()))
                (while (and available (< (length selected) count))
                  (let ((candidate (pop available)))
                    (unless (or (member candidate selected)
                                (seq-some (lambda (combo)
                                            (seq-some (lambda (x) (equal x candidate)) combo))
                                          previous-combinations))
                      (push candidate selected))))
                (when (< (length selected) count)
                  (let ((fallbacks (shuffle-list (copy-sequence (gethash tag all-tag-headings)))))
                    (while (and fallbacks (< (length selected) count))
                      (let ((extra (pop fallbacks)))
                        (unless (member extra selected)
                          (push extra selected))))))
                (setq combo (append combo selected))))
            (setq combo (shuffle-list combo))
            (unless (seq-some (lambda (prev)
                                (equal (normalize-combination combo)
                                       (normalize-combination prev)))
                              previous-combinations)
              (setq final-combination combo)
              (setq unique-combo-found t))))

        ;; Step 5: Display result or error
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
                  (insert header-line "\n")
                  (let ((prev-line-type nil))
                    (dolist (line (cdr heading))
                      (cond
                       ((string-match-p "^\\s-*:PROPERTIES:" line)
                        (insert line "\n")
                        (setq prev-line-type 'prop))
                       ((string-match-p "^\\s-*:END:" line)
                        (insert line "\n")
                        (setq prev-line-type 'prop-end))
                       ((string-match-p "^\\s-*:.*" line)
                        (insert line "\n")
                        (setq prev-line-type 'prop))
                       ((string-match-p "^\\*+" line)
                        (when (eq prev-line-type 'prop-end)
                          (insert "\n"))
                        (insert line "\n")
                        (setq prev-line-type 'meta))
                       (t
                        ;; Add blank line after properties
                        (when (memq prev-line-type '(prop prop-end))
                          (insert "\n"))
                        ;; Add blank line between content paragraphs
                        (when (and (eq prev-line-type 'content)
                                   (not (string-blank-p line)))
                          (insert "\n"))
                        (insert line "\n")
                        (setq prev-line-type 'content))))))
                (insert "\n")))
            (switch-to-buffer buffer)
            (message "Unique combination generated in %d attempt(s)." attempt)))))))

(provide 'mcq-persistent-org-combo-shuffler)