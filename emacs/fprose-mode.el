;;   Copyright 2019 Google LLC
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       https://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

;; Hook that allows users to run their code when we enter fprose mode.
(defvar fprose-mode-hook nil)

;; Keymap used in fprose mode.
(defvar fprose-mode-map
  (let ((map (make-keymap)))
    (define-key map "\M-." 'fprose-hide-more)
    (define-key map "\M-," 'fprose-hide-less)
    (define-key map "\"" 'fprose-insert-or-cycle-double-quote)
    (define-key map "'" 'fprose-insert-or-cycle-single-quote)
    (define-key map "-" 'fprose-insert-or-cycle-hyphen)
    (define-key map "\M-u" 'fprose-unhide-string)
    map)
  "Keymap for Fprose major mode")

;; True if the line starting at point is a grouping line.
(defun fprose--line-is-grouping () (eq (char-after) ?\+) )

;; The label for the grouping line starting at point, or nil if it isn't a
;; grouping line.
(defun fprose--line-grouping-label ()
  (if (looking-at "\\+\\([[:alnum:]]+\\)")
      (match-string-no-properties 1)
    nil)
  )

(defun fprose-replace-prev-char (c)
  (delete-backward-char 1)
  (insert c)
  )

(defun fprose-insert-or-cycle-double-quote ()
  (interactive)
  (let ((cycle "(press '\"' again to cycle)"))
    (cond
     ;; Cycle from straight double quote to open double quote:
     ((eq (char-before) 34)
      (fprose-replace-prev-char 8220)
      (message "Open double quote %s" cycle))
     ;; Cycle from open double quote to close double quote:
     ((eq (char-before) 8220)
      (fprose-replace-prev-char 8221)
      (message "Close double quote %s" cycle))
     ;; Cycle from close double quote to straight double quote:
     ((eq (char-before) 8221)
      (fprose-replace-prev-char 34)
      (message "Straight double quote %s" cycle))
     ;; Choose to insert open or closed depending on context:
     (t (if
	    (or (bobp)
		(eq (char-before) 8216) ; open single quote
		(eq (char-before) 9) ; tab
		(eq (char-before) 10) ; newline
		(eq (char-before) 13) ; carriage return
		(eq (char-before) 32) ; space
		)
	    (progn
	      (insert 8220) ; open double quote
	      (message "Open double quote %s" cycle))
	  (progn
	    (insert 8221) ; close double quote
	    (message "Close double quote %s" cycle))
	  )
	)
     )
    )
  )

(defun fprose-insert-or-cycle-single-quote ()
  (interactive)
  (let ((cycle "(press \"'\" again to cycle)"))
    (cond
     ;; Cycle from straight single quote to open single quote:
     ((eq (char-before) 39)
      (fprose-replace-prev-char 8216)
      (message "Open single quote %s" cycle))
     ;; Cycle from open single quote to close single quote:
     ((eq (char-before) 8216)
      (fprose-replace-prev-char 8217)
      (message "Close single quote %s" cycle))
     ;; Cycle from close single qote to straight single quote:
     ((eq (char-before) 8217)
      (fprose-replace-prev-char 39)
      (message "Straight single quote %s" cycle))
     ;; Choose to insert open or closed depending on context:
     (t (if
	    (or (bobp)
		(eq (char-before) 8220) ; open double quote
		(eq (char-before) 9) ; tab
		(eq (char-before) 10) ; newline
		(eq (char-before) 13) ; carriage return
		(eq (char-before) 32) ; space
		)
	    (progn
	      (insert 8216) ; open double quote
	      (message "Open single quote %s" cycle))
	  (progn
	    (insert 8217) ; close double quote
	    (message "Close single quote %s" cycle))
	  )
	)
     )
    )
  )

(defun fprose-insert-or-cycle-hyphen ()
  (interactive)
  (let ((cycle "(press \"-\" again to cycle)"))
    (cond
     ;; Cycle from minus/hyphen to em dash:
     ((eq (char-before) 45)
      (fprose-replace-prev-char 8212)
      (message
       "em dash (sentence splits) %s" cycle))
     ;; Cycle from em dash to en dash:
     ((eq (char-before) 8212)
      (fprose-replace-prev-char 8211)
      (message
       "en dash (ranges, nested compound terms) %s" cycle))
     ;; Cycle from en dash to minus/hyphen:
     ((eq (char-before) 8211)
      (fprose-replace-prev-char 45)
      (message
       "minus/hyphen (math, compound terms) %s" cycle))
     ;; Insert minus/hyphen:
     (t (insert 45)
	(message
	 "minus/hyphen (math, compound terms) %s" cycle))
     )
    )
  )

;; The grouping level for the line starting at point, or nil if it isn't a
;; grouping line or if it's not a valid grouping level.
(defun fprose--line-grouping-level (grouping-levels)
  (let ((r (assoc (fprose--line-grouping-label) grouping-levels)))
    (if r (elt r 1) 1)
    )
  )

;; True if the line starting at point is a comment line.
(defun fprose--line-is-comment () (eq (char-after) ?\!) )

;; The label for the comment line starting at point, or nil if it isn't a
;; comment line.
(defun fprose--line-comment-label ()
  (if (looking-at "\\!\\([[:alnum:]]+\\)")
      (match-string-no-properties 1)
    nil)
  )

;; The grouping level for the line starting at point, or nil if it isn't a
;; grouping line or if it's not a valid grouping level.
(defun fprose--line-comment-level (comment-levels)
  (let ((r (assoc (fprose--line-comment-label) comment-levels)))
    (if r (elt r 1) 1)
    )
  )

;; True if the line starting at point is a config line.
(defun fprose--line-is-config () (eq (char-after) ?\@) )

;; True if the line starting at point is a prose line, which define as any line
;; that is not group, comment, or config.
(defun fprose--line-is-prose ()
  (not
   (or (fprose--line-is-grouping)
       (fprose--line-is-comment)
       (fprose--line-is-config)
       )
   )
  )

;; The current level of hiding.
(setq fprose-hide-level 0)

;; True if the line starting at point should be hidden at the current hiding
;; levels.
(defun fprose--should-hide-line (grouping-levels comment-levels show-string)
  (cond
   ((and show-string (looking-at-p (concat ".*" (regexp-quote show-string)))) nil)
   ((fprose--line-is-prose) (> fprose-hide-level 0))
   ((fprose--line-is-grouping)
    (let ((line-level (fprose--line-grouping-level grouping-levels)))
      (<= line-level fprose-hide-level)))
   ((fprose--line-is-comment)
    (let ((line-level (fprose--line-comment-level comment-levels)))
      (<= line-level fprose-hide-level)))
   (t nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fprose\\'" . fprose-mode))

(defconst fprose-font-lock-keywords
  (list
   '("^!.*$" . font-lock-comment-face)
   '("^+.*$" . font-lock-function-name-face)
   '("^@.*$" . font-lock-constant-face)
   )
  "Default highlighting for fprose mode"
  )

(defun fprose-mode ()
  "Major mode for editing Fprose files"
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults)
       '(fprose-font-lock-keywords t))
  (set (make-local-variable 'fprose-hide-level) 0)
  (set (make-local-variable 'fprose-hide-level) 0)
  (setq major-mode 'fprose-mode)
  (use-local-map fprose-mode-map)
  (setq mode-name "fprose")
  (visual-line-mode)
  (run-hooks 'fprose-mode-hook))

(defun fprose--hidden-label (words)
  (if (> words 0)
      (format "<... %d words ...>" words)
    "<...>")
  )

(defun fprose--hide-region (begin end words)
  (when begin
    (let ((o (make-overlay begin end nil t nil)))
      (overlay-put o 'type 'fprose-hidden)
      (overlay-put o 'invisible t)
    (overlay-put o 'face 'font-lock-builtin-face)
    (overlay-put o 'display
		 (propertize (fprose--hidden-label words)
			     'face 'font-lock-string-face))
    (overlay-put o 'evaporate t))
    (deactivate-mark))
  )

(defun fprose--delete-my-overlay (it)
  (when (eq (overlay-get it 'type) 'fprose-hidden)
    (delete-overlay it)))

(defun fprose--find-grouping-and-comment-levels ()
  (let ((grouping-levels ())
	(comment-levels ()))
    (save-excursion
      (goto-char (point-min))
      (while (looking-at "@\\|[[:space:]]*$")
	(cond ((looking-at "\\@GROUPING \\([[:alnum:]]+\\).*hidelevel=\\([[:digit:]]+\\)")
	       (setq grouping-levels (cons
				      (list (match-string-no-properties 1) (string-to-number (match-string-no-properties 2)))
				      grouping-levels)))
	      ((looking-at "\\@COMMENT \\([[:alnum:]]+\\).*hidelevel=\\([[:digit:]]+\\)")
	       (setq comment-levels (cons
				     (list (match-string-no-properties 1) (string-to-number (match-string-no-properties 2)))
				     comment-levels)))
	      )
	(forward-line 1)
	)
    (list grouping-levels comment-levels)
    )
    )
  )

(defun fprose--update-display-level (grouping-levels comment-levels show-string)
  ; First unhide everything.
  (mapc 'fprose--delete-my-overlay
        (overlays-in (point-min) (point-max)))
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp)) (not (fprose--line-is-grouping)))
      (forward-line 1)
      )
    (previous-line)
    (end-of-line)
    (let ((begin nil) (end nil) (words 0))
      (while (not (eobp))
	(let ((end-of-prev-line (point-marker)))
	  (forward-line 1)
	  (if (not (fprose--should-hide-line grouping-levels comment-levels show-string))
	      (progn
		(fprose--hide-region begin end words)
		(setq begin nil)
		(setq end nil)
		(end-of-line)
		(setq words 0)
		)
	    (let ((is-prose (fprose--line-is-prose)))
	      (setq begin (or begin end-of-prev-line))
	      (end-of-line)
	      (setq end (point-marker))
	      (when is-prose
		  (setq words (+ words
				 (count-words-region
				  end-of-prev-line end))))
	      )
	    )
	  ) ; let
	) ; while
      (fprose--hide-region begin end words)
      ) ; let
    ) ; save-excursion
  ) ; defun

(defun fprose--labels-at-or-below-level (levels level)
  (cond ((and levels (<= (elt (car levels) 1) level))
	 (cons (car (car levels)) (fprose--labels-at-or-below-level (cdr levels) level)))
	(levels (fprose--labels-at-or-below-level (cdr levels) level))
	(t nil))
  )

(defun fprose--max-level (levels sofar)
  (cond ((and levels (> (elt (car levels) 1) sofar))
	 (fprose--max-level (cdr levels) (elt (car levels) 1)))
	(levels (fprose--max-level (cdr levels) sofar))
	(t sofar))
  )

(defun fprose--describe-hide-level (grouping-levels comment-levels)
  (if (eq 0 fprose-hide-level) "Hide level 0: Show all"
    (format "Hide level %d: Hide prose%s%s" fprose-hide-level
	    (mapconcat (lambda (x) (concat " !" x))
		       (fprose--labels-at-or-below-level comment-levels fprose-hide-level) "")
	    (mapconcat (lambda (x) (concat " +" x))
		       (fprose--labels-at-or-below-level grouping-levels fprose-hide-level) "")
	    )
    )
  )

(defun fprose-hide-more ()
  (interactive)
  (let* ((levels (fprose--find-grouping-and-comment-levels))
	(grouping-levels (elt levels 0))
	(comment-levels (elt levels 1))
	(max-hide-level (max (fprose--max-level grouping-levels 1) (fprose--max-level comment-levels 1))))
    (setq fprose-hide-level (max 0 (min (+ fprose-hide-level 1) max-hide-level)))
    (fprose--update-display-level grouping-levels comment-levels nil)
    (message (fprose--describe-hide-level grouping-levels comment-levels))
    )
  )

(defun fprose-hide-less ()
  (interactive)
  (let* ((levels (fprose--find-grouping-and-comment-levels))
	(grouping-levels (elt levels 0))
	(comment-levels (elt levels 1))
	(max-hide-level (max (fprose--max-level grouping-levels 1) (fprose--max-level comment-levels 1))))
    (setq fprose-hide-level (max 0 (min (- fprose-hide-level 1) max-hide-level)))
    (fprose--update-display-level grouping-levels comment-levels nil)
    (message (fprose--describe-hide-level grouping-levels comment-levels))
    )
  )

(defun fprose-unhide-string (s)
  (interactive "sUnhide string: ")
  (let* ((levels (fprose--find-grouping-and-comment-levels))
	 (grouping-levels (elt levels 0))
	 (comment-levels (elt levels 1)))
    (fprose--update-display-level grouping-levels comment-levels s)
    )
  )

(defun fprose-unhide-last-search ()
  (interactive)
  (if search-ring
      (fprose-unhide-string (car search-ring))
    (message "No previous search string.")
    )
  )

(provide 'fprose-mode)
