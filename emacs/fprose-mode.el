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
    (define-key map "\"" 'fprose-insert-or-cycle-double-quote)
    (define-key map "'" 'fprose-insert-or-cycle-single-quote)
    (define-key map "-" 'fprose-insert-or-cycle-hyphen)
    (define-key map "\M-." 'fprose-follow-link)
    map)
  "Keymap for Fprose major mode")

(defun fprose--replace-prev-char (c)
  (delete-backward-char 1)
  (insert c)
  )

(defun fprose-insert-or-cycle-double-quote ()
  (interactive)
  (let ((cycle "(press '\"' again to cycle)"))
    (cond
     ;; Cycle from straight double quote to open double quote:
     ((eq (char-before) 34)
      (fprose--replace-prev-char 8220)
      (message "Open double quote %s" cycle))
     ;; Cycle from open double quote to close double quote:
     ((eq (char-before) 8220)
      (fprose--replace-prev-char 8221)
      (message "Close double quote %s" cycle))
     ;; Cycle from close double quote to straight double quote:
     ((eq (char-before) 8221)
      (fprose--replace-prev-char 34)
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
      (fprose--replace-prev-char 8216)
      (message "Open single quote %s" cycle))
     ;; Cycle from open single quote to close single quote:
     ((eq (char-before) 8216)
      (fprose--replace-prev-char 8217)
      (message "Close single quote %s" cycle))
     ;; Cycle from close single qote to straight single quote:
     ((eq (char-before) 8217)
      (fprose--replace-prev-char 39)
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
      (fprose--replace-prev-char 8212)
      (message
       "em dash (sentence splits) %s" cycle))
     ;; Cycle from em dash to en dash:
     ((eq (char-before) 8212)
      (fprose--replace-prev-char 8211)
      (message
       "en dash (ranges, nested compound terms) %s" cycle))
     ;; Cycle from en dash to minus/hyphen:
     ((eq (char-before) 8211)
      (fprose--replace-prev-char 45)
      (message
       "minus/hyphen (math, compound terms) %s" cycle))
     ;; Insert minus/hyphen:
     (t (insert 45)
	(message
	 "minus/hyphen (math, compound terms) %s" cycle))
     )
    )
  )

; Current string we're folded to.  Buffer local.
(setq fprose-folded-to-string nil)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fprose\\'" . fprose-mode))

(defconst fprose-font-lock-keywords
  (list
   '("^![[:alpha:]_]+" . font-lock-comment-face)
   '("^++[[:alpha:]_]+" . font-lock-function-name-face)
   '("{[[:alnum:]_]+}" . font-lock-constant-face)
   '("\\[[[:alnum:]_]+\\]" . font-lock-constant-face)

   )
  "Default highlighting for fprose mode"
  )

(defun fprose-mode ()
  "Major mode for editing Fprose files"
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults)
       '(fprose-font-lock-keywords t))
  (set (make-local-variable 'fprose-folded-to-string) nil)
  (setq major-mode 'fprose-mode)
  (use-local-map fprose-mode-map)
  (setq mode-name "fprose")
  (visual-line-mode)
  (add-hook 'isearch-mode-end-hook 'fprose-end-isearch nil t)
  (add-hook 'isearch-update-post-hook 'fprose-fold-to-lines-containing-isearch nil t)
  (run-hooks 'fprose-mode-hook))

(defun fprose--hide-region (begin end num)
  (when begin
    (let ((o (make-overlay begin end nil t nil)))
      (overlay-put o 'type 'fprose-hidden)
      (overlay-put o 'invisible t)
    (overlay-put o 'face 'font-lock-builtin-face)
    (overlay-put o 'display
		 (propertize "<...>" 'face 'font-lock-string-face))
    (overlay-put o 'evaporate t))
    (deactivate-mark))
  )

(defun fprose--delete-my-overlay (it)
  (when (eq (overlay-get it 'type) 'fprose-hidden)
    (delete-overlay it)))

(defun fprose--delete-all-overlays ()
  (mapc 'fprose--delete-my-overlay
        (overlays-in (point-min) (point-max)))
  )

(defun fprose-fold-to-lines (l)
  (fprose--delete-all-overlays)
  (when l
    (let ((prev-end 1)
	  (cur l)
	  (num 0))
      (while cur
	(when (> (elt (car cur) 0) prev-end)
	  (fprose--hide-region prev-end (elt (car cur) 0) num)
	  (setq num (+ num 1))
	  ) ; when
	(setq prev-end (elt (car cur) 1))
	(setq cur (cdr cur))
	) ; while
      (when (< prev-end (point-max))
	(fprose--hide-region prev-end (point-max) num)
	) ; when
      ) ; let
    (recenter)
    ); when
  )

(defun fprose-fold-to-lines-containing-string (s)
  (interactive "sUnhide string: ")
  (fprose-fold-to-lines (fprose-lines-containing-string s))
  )

(defun fprose-fold-to-lines-containing-isearch ()
  (when (not (string-equal isearch-string fprose-folded-to-string))
    (fprose-fold-to-lines (fprose-lines-containing-string-plus-context isearch-string) )
    (setq fprose-folded-to-string isearch-string)
    )
  )

(defun fprose-end-isearch ()
  (fprose--delete-all-overlays)
  (setq fprose-folded-to-string nil)
  )
(defun fprose--section-depth (p)
  (let ((result 0))
    (while (eq (char-after (+ p result)) ?\+)
      (setq result (+ result 1))
      )
    result
    )
  )

(defun fprose-section-lines ()
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^++[[:alpha:]_]+" nil t)
	(let* ((line-begin (line-beginning-position))
	       (line-end (line-end-position))
	       (depth (fprose--section-depth line-begin)))
	  (push (list (- line-begin 1) line-end depth) result)
	  (end-of-line)
	  ) ; let*
	) ; while
      ) ; save-excursion
    (reverse result)
    ) ; let
  )

(defun fprose-lines-containing-string (s)
  (let ((result nil))
    (when (and s (> (length s) 0))
      (save-excursion
	(goto-char (point-min))
	(while (search-forward s nil t)
	  (push (list (- (line-beginning-position) 1) (line-end-position)) result)
	  (end-of-line)
	  ) ; while
	) ; save-excursion
      ); when
    (reverse result)
    ) ; let
  )

(defun fprose-add-unprinted-from-stack (r s)
  (if (or (not s) (and r (<= (elt (car s) 0) (elt (car r) 0))))
      r
    (cons (list (elt (car s) 0) (elt (car s) 1)) (fprose-add-unprinted-from-stack r (cdr s)))
    )
  )

(defun fprose-get-square-link-name()
  (cond ((looking-at "\\[[[:alnum:]_]+\\]")
	 (buffer-substring (+ (match-beginning 0) 1) (- (match-end 0) 1))
	 )
	((looking-at "\\]")
	 (let ((e (match-end 0)))
	   (if (looking-back "\\[[[:alnum:]_]+" 50)
	       (buffer-substring (+ (match-beginning 0) 1) (- e 1))
	     nil
	     )
	   )
	 )
	((looking-at "[[:alnum:]_]+\\]")
	 (let ((e (match-end 0)))
	   (if (looking-back "\\[[[:alnum:]_]*" 50)
	       (buffer-substring (+ (match-beginning 0) 1) (- e 1))
	     nil
	     )
	   )
	 )
	(t nil)
	)
  )

(defun fprose-get-curly-link-name()
  (cond ((looking-at "\{[[:alnum:]_]+\}")
	 (buffer-substring (+ (match-beginning 0) 1) (- (match-end 0) 1))
	 )
	((looking-at "\}")
	 (let ((e (match-end 0)))
	   (if (looking-back "\{[[:alnum:]_]+" 50)
	       (buffer-substring (+ (match-beginning 0) 1) (- e 1))
	     nil
	     )
	   )
	 )
	((looking-at "[[:alnum:]_]+\}")
	 (let ((e (match-end 0)))
	   (if (looking-back "\{[[:alnum:]_]*" 50)
	       (buffer-substring (+ (match-beginning 0) 1) (- e 1))
	     nil
	     )
	   )
	 )
	(t nil)
	)
  )

(defun fprose-start-isearch (s)
  (isearch-forward nil 1)
  (isearch-yank-string s)
  )

(defun fprose-isearch-references (d)
  (fprose-start-isearch (format "[%s]" d))
  )

(defun fprose-goto-definition (d)
  (beginning-of-buffer)
  (search-forward (format "{%s}" d))
  )

(defun fprose-follow-link ()
  (interactive)
  (let ((square (fprose-get-square-link-name)))
    (if square
	(fprose-goto-definition square)
      (let ((curly (fprose-get-curly-link-name)))
	(if curly
	    (fprose-isearch-references curly)
	  (message "No link or definition here.")
	  )
	)
      )
    )
  )

(defun fprose-lines-containing-string-plus-context (s)
  (let ((result nil)
	(section-lines (fprose-section-lines))
	(stack nil))
    (when (and s (> (length s) 0))
      (save-excursion
	(goto-char (point-min))
	(while (search-forward s nil t)
	  (let* ((line-begin (line-beginning-position))
		 (line-pre-begin (- line-begin 1))
		 (line-end (line-end-position)))
	    (while (and section-lines (< (elt (car section-lines) 0) line-pre-begin))
	      (while (and stack (>= (elt (car stack) 2) (elt (car section-lines) 2)))
		(pop stack)
		); while
	      (push (car section-lines) stack)
	      (setq section-lines (cdr section-lines))
	      ) ; while
	    (when (and section-lines (= (elt (car section-lines) 0) line-pre-begin))
	      (setq section-lines (cdr section-lines))
	      ) ; while
	    ; TODO: print any unprinted things in the stack.
	    (setq result (fprose-add-unprinted-from-stack result stack))
	    (push (list line-pre-begin line-end) result)
	    (end-of-line)
	    )
	  ) ; while
	) ; save-excursion
      ); when
    (reverse result)
    ) ; let
  )

(provide 'fprose-mode)
