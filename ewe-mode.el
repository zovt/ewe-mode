;;; ewe-mode --- Edit With Emotion
;;; Commentary:
;;;;
;;; Code:

;; Basic defines
(defvar ewe-mode-hook nil)

;; Mode vars
(defgroup ewe-group nil "Customization gruop for EWE.")
(defcustom ewe-max-regex-length 100 "Max regex length for searching. Setting this too high can cause lag, but setting it too low can cause matching errors." :group 'ewe-group)

;;; Set up keymap
(defvar ewe-mode-map (make-keymap) "Keymap for EWE major mode.")

;;;; Move Commands
(define-key ewe-mode-map (kbd "C-S-p") 'ewe-move-beginning-of-paragraph)
(define-key ewe-mode-map (kbd "M-p") 'ewe-move-beginning-of-paragraph)
(define-key ewe-mode-map (kbd "C-S-n") 'ewe-move-end-of-paragraph)
(define-key ewe-mode-map (kbd "M-n") 'ewe-move-end-of-paragraph)
(define-key ewe-mode-map (kbd "C-S-f") 'ewe-move-next-word)
(define-key ewe-mode-map (kbd "C-S-b") 'ewe-move-previous-word)

(define-key ewe-mode-map (kbd "C-. t") 'ewe-move-to-char)
(define-key ewe-mode-map (kbd "C-. r") 'ewe-move-forward-regex)
(define-key ewe-mode-map (kbd "C-. C-r") 'ewe-move-forward-regex)
(define-key ewe-mode-map (kbd "C-. R") 'ewe-move-back-regex)
(define-key ewe-mode-map (kbd "C-. C-S-r") 'ewe-move-back-regex)

;;;; Kill Commands
(define-key ewe-mode-map (kbd "C-k l") 'ewe-kill-line)
(define-key ewe-mode-map (kbd "C-k k") 'ewe-kill-line)
(define-key ewe-mode-map (kbd "C-k C-k") 'kill-line)
(define-key ewe-mode-map (kbd "C-k e") 'ewe-kill-end-of-line)
(define-key ewe-mode-map (kbd "C-k p") 'ewe-kill-paragraph)
(define-key ewe-mode-map (kbd "C-k w") 'ewe-kill-word)
(define-key ewe-mode-map (kbd "C-S-d") 'ewe-kill-word)
(define-key ewe-mode-map (kbd "C-k t") 'ewe-kill-until-char)
(define-key ewe-mode-map (kbd "C-k T") 'ewe-kill-through-char)

;;;; Yank Commands
(define-key ewe-mode-map (kbd "M-k l") 'ewe-yank-line)
(define-key ewe-mode-map (kbd "M-k k") 'ewe-yank-line)
(define-key ewe-mode-map (kbd "M-k M-k") 'ewe-yank-line)
(define-key ewe-mode-map (kbd "M-k p") 'ewe-yank-paragraph)
(define-key ewe-mode-map (kbd "M-k w") 'ewe-yank-word)
(define-key ewe-mode-map (kbd "M-S-d") 'ewe-yank-word)
(define-key ewe-mode-map (kbd "M-k t") 'ewe-yank-until-char)
(define-key ewe-mode-map (kbd "M-k T") 'ewe-yank-through-char)

;; Functions
;;; Utility
(defun ewe-match-regexp-before (pos expr &optional max-length)
  "Match EXPR before POS and return the position. MAX-LENGTH is *very* useful for optimization."
  (let ((chars-back 0))
    (if max-length
	(progn
	  (while (not (or (eq (- pos chars-back) (point-min))
			  (string-match-p expr
					  (buffer-substring (- pos chars-back)
							    (+ (- pos chars-back) max-length)))))
	    (setq chars-back (+ chars-back 1))))
      (progn
	(while (not (or (string-match-p expr
					(buffer-substring (- pos chars-back)
							  pos))
			(eq (- pos chars-back) (point-min))))
	  (setq chars-back (+ chars-back 1)))))
    (- pos chars-back)))

(defun ewe-match-regexp-after (pos expr &optional max-length)
  "Match EXPR after POS and return the position. MAX-LENGTH is *very* useful for optimization."
  (let ((chars-forward (if max-length
			   max-length
			 0))
	(string-pos 0)
	(keep-going 1)
	(string ""))
    (while (and (< (+ pos chars-forward) (point-max)) keep-going)
      (progn
	(if max-length
	    (setq string (buffer-substring (+ pos (- chars-forward max-length))
					   (+ pos chars-forward)))
	  (setq string (buffer-substring pos
					 (+ pos chars-forward))))
	(if (and (string-match-p expr string)
		 (if max-length
		     (not (equal 0 (- chars-forward max-length)))
		   (not (equal 0 chars-forward))))
	    (progn (setq keep-going nil) (setq string-pos (string-match expr string)))
	  (setq chars-forward (+ chars-forward 1)))))
    (if max-length
	(+ pos (- chars-forward max-length) string-pos)
      (+ pos string-pos))))

(defun ewe-find-beginning-of-paragraph (&optional offset)
  "Find beginning of paragraph and return position. OFFSET is optional and used to jump paragraph."
  (interactive)
  (if (not offset)
      (setq offset 0))
  (let ((lines-back (+ offset 1)))
    (while (not (or (equal (char-after (line-beginning-position (- 0 lines-back))) ?\n) (equal (line-beginning-position (- 0 lines-back)) (point-min))))
      (setq lines-back (+ lines-back 1)))
    (line-beginning-position (- 0 lines-back))))

(defun ewe-find-end-of-paragraph (&optional offset)
  "Find end of paragraph and return position. OFFSET is optional and used to jump paragraph."
  (interactive)
  (if (not offset)
      (setq offset 0))
  (let ((lines-forward (+ offset 1)))
    (while (not (or (equal (char-after (line-beginning-position lines-forward)) ?\n) (equal (line-beginning-position lines-forward) (point-max))))
      (setq lines-forward (+ lines-forward 1)))
    (line-beginning-position lines-forward)))

(defun ewe-find-beginning-of-word (&optional offset)
  "Find the beginning of the word from OFFSET."
  (interactive)
  (if (not offset)
      (setq offset 0))
  (let ((chars-back offset)
	(pos (point)))
    (while (not (or (member (char-after (- pos chars-back)) '(? ?? ?! ?\( ?\) ?' ?\\ ?/ ?\" ?. ?, ?\; ?: ?\n ?-)) (equal (- pos chars-back) (point-min))))
      (setq chars-back (+ chars-back 1)))
    (- pos (- chars-back 1))))

(defun ewe-find-end-of-word (&optional offset)
  "Find the end of the word from OFFSET."
  (interactive)
  (if (not offset)
      (setq offset 0))
  (let ((chars-forward offset)
	(pos (point)))
    (while (not (or (member (char-after (+ pos chars-forward)) '(? ?? ?! ?\( ?\) ?' ?\\ ?/ ?- ?\" ?. ?, ?\; ?: ?\n)) (equal (+ pos chars-forward) (point-max))))
      (setq chars-forward (+ chars-forward 1)))
    (+ pos chars-forward)))

(defun ewe-find-letter (&optional offset)
  "Find letter from OFFSET."
  (if (not offset)
      (setq offset 0))
  (let ((chars-forward offset)
	(pos (point))
	(key-char (read-char)))
    (while (not (or (equal (char-after (+ pos chars-forward)) key-char) (equal (+ pos chars-forward) (point-max))))
      (setq chars-forward (+ chars-forward 1)))
    (+ pos chars-forward)))

;;; Movement
(defun ewe-move-beginning-of-paragraph ()
  "Move to beginning of paragraph."
  (interactive)
  (goto-char (ewe-find-beginning-of-paragraph 1)))

(defun ewe-move-end-of-paragraph ()
  "Move to end of paragraph."
  (interactive)
  (goto-char (ewe-find-end-of-paragraph 1)))

(defun ewe-move-next-word ()
  "Move to the end of word."
  (interactive)
  (goto-char (+ (ewe-find-end-of-word) 1)))

(defun ewe-move-previous-word ()
  "Move to the end of word."
  (interactive)
  (goto-char (ewe-find-beginning-of-word 2)))

(defun ewe-move-to-char ()
  "Move to specified char in line."
  (interactive)
  (goto-char (ewe-find-letter 1)))

(defun ewe-move-forward-regex ()
  "Move forward to regex."
  (interactive)
  (let ((reg (read-from-minibuffer "Regex: ")))
    (goto-char (ewe-match-regexp-after (point) reg ewe-max-regex-length))))

(defun ewe-move-back-regex ()
  "Move backward to regex."
  (interactive)
  (let ((reg (read-from-minibuffer "Regex: ")))
    (goto-char (ewe-match-regexp-before (point) reg ewe-max-regex-length))))

;;; Kill
(defun ewe-kill-line ()
  "Kill entire line."
  (interactive)
  (kill-region (line-beginning-position) (line-beginning-position 2)))

(defun ewe-kill-end-of-line ()
  "Kill until end of line."
  (interactive)
  (kill-region (point) (line-end-position)))

(defun ewe-kill-paragraph ()
  "Kill a paragraph."
  (interactive)
  (kill-region (ewe-find-beginning-of-paragraph) (ewe-find-end-of-paragraph)))

(defun ewe-kill-word ()
  "Kill a word."
  (interactive)
  (kill-region (ewe-find-beginning-of-word) (ewe-find-end-of-word)))

(defun ewe-kill-until-char ()
  "Kill until a character."
  (interactive)
  (kill-region (point) (ewe-find-letter 1)))

(defun ewe-kill-through-char ()
  "Kill up to and including a character."
  (interactive)
  (kill-region (point) (+ (ewe-find-letter 1) 1)))

;;; Yank
(defun ewe-yank-line ()
  "Yank entire line."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

(defun ewe-yank-paragraph ()
  "Yank entire paragraph."
  (interactive)
  (kill-ring-save (ewe-find-beginning-of-paragraph) (ewe-find-end-of-paragraph)))

(defun ewe-yank-word ()
  "Yank word."
  (interactive)
  (kill-ring-save (ewe-find-beginning-of-word) (ewe-find-end-of-word)))

(defun ewe-yank-until-char ()
  "Yank up to char."
  (interactive)
  (kill-ring-save (point) (ewe-find-letter 1)))

(defun ewe-yank-through-char ()
  "Yank up to and including char."
  (interactive)
  (kill-ring-save (point) (+ (ewe-find-letter 1) 1)))

;; Mode
(define-minor-mode ewe-minor-mode
  "Toggle EWE Minor Mode."
  :lighter " EWE"
  :keymap ewe-mode-map)

(define-globalized-minor-mode ewe-global-mode ewe-minor-mode ewe-minor-mode)

(provide 'ewe-mode)
;;; ewe-mode.el ends here
