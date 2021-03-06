;;; ewe-mode --- Edit With Emotion
;;; Commentary:
;;;;
;;; Code:

;; Basic defines
(defvar ewe-mode-hook nil)

;; Mode vars
(defgroup ewe-group nil "Customization gruop for EWE.")
(defcustom ewe-pairs '((?\( . ?\))
		       (?\" . ?\")
		       (?' . ?')
		       (?{ . ?})
		       (?< . ?>))
  "Pairs used for deleting inside and around of pairs.")


;;; Set up keymap
(defvar ewe-mode-map (make-keymap) "Keymap for EWE major mode.")

;;;; Move Commands
(define-key ewe-mode-map (kbd "C-S-p") 'ewe-move-beginning-of-paragraph)
(define-key ewe-mode-map (kbd "M-p") 'ewe-move-beginning-of-paragraph)
(define-key ewe-mode-map (kbd "C-S-n") 'ewe-move-end-of-paragraph)
(define-key ewe-mode-map (kbd "M-n") 'ewe-move-end-of-paragraph)
(define-key ewe-mode-map (kbd "C-S-f") 'ewe-move-next-word)
(define-key ewe-mode-map (kbd "C-S-b") 'ewe-move-previous-word)
(define-key ewe-mode-map (kbd "C-M-n") 'ewe-move-forward-delimiter)
(define-key ewe-mode-map (kbd "C-M-p") 'ewe-move-back-delimiter)

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
(define-key ewe-mode-map (kbd "C-k i") 'ewe-kill-inside-pair)
(define-key ewe-mode-map (kbd "C-k I") 'ewe-kill-around-pair)

;;;; Yank Commands
(define-key ewe-mode-map (kbd "M-k l") 'ewe-yank-line)
(define-key ewe-mode-map (kbd "M-k k") 'ewe-yank-line)
(define-key ewe-mode-map (kbd "M-k M-k") 'ewe-yank-line)
(define-key ewe-mode-map (kbd "M-k p") 'ewe-yank-paragraph)
(define-key ewe-mode-map (kbd "M-k w") 'ewe-yank-word)
(define-key ewe-mode-map (kbd "M-S-d") 'ewe-yank-word)
(define-key ewe-mode-map (kbd "M-k t") 'ewe-yank-until-char)
(define-key ewe-mode-map (kbd "M-k T") 'ewe-yank-through-char)
(define-key ewe-mode-map (kbd "M-k i") 'ewe-yank-inside-pair)
(define-key ewe-mode-map (kbd "M-k I") 'ewe-yank-around-pair)

;;;; Modes
;; (define-key ewe-mode-map (kbd "C-s") 'ewe-regex-match-mode)

;; Functions
;;; Utility
(defun ewe-match-regex-before (regex pos)
  "Match REGEX before POS."
  (interactive)
  (let ((str (buffer-substring (point-min) pos))
	(start (point-min))
	(position pos)
	(res pos))
    (while (setq position (string-match regex str start))
      (setq start (+ position 1))
      (setq res start))
    res))

(defun ewe-match-regex-after (regex pos)
  "Match REGEX after POS."
  (interactive)
  (let ((str (buffer-substring pos (point-max)))
	(position pos))
    (if (setq position (string-match regex str))
	(+ pos position)
      pos)))

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

(defun ewe-find-letter-forward (&optional offset)
  "Find letter forward from OFFSET."
  (if (not offset)
      (setq offset 0))
  (let ((chars-forward offset)
	(pos (point))
	(key-char (read-char)))
    (while (not (or (equal (char-after (+ pos chars-forward)) key-char) (equal (+ pos chars-forward) (point-max))))
      (setq chars-forward (+ chars-forward 1)))
    (+ pos chars-forward)))

(defun ewe-find-letter-backward (&optional offset)
  "Find letter backward from OFFSET."
  (if (not offset)
      (setq offset 0))
  (let ((chars-backward offset)
	(pos (point))
	(key-char (read-char)))
    (while (not (or (equal (char-after (- pos chars-backward)) key-char) (equal (- pos chars-backward) (point-min))))
      (setq chars-backward (+ chars-backward 1)))
    (+ pos chars-backward)))

(defun ewe-check-if-pair (char)
  "Check if CHAR is defined as pair."
  (interactive)
  (if (or (equal (char-syntax char) ?\() (equal (char-syntax char) ?\)))
      t
    nil))

(defun ewe-find-left-delimiter (&optional pos offset)
  "Find the left (opening) delimiter of a pair at POS (point by default) and OFFSET (0 by default). Returns a position."
  (interactive)
  (or offset
      (setq offset 0))
  (or pos
      (setq pos (+ (point) offset)))
  (let* ((depth (car-safe (syntax-ppss pos))))
    (if (> depth 0)
	(elt (syntax-ppss pos) 1)
      (- pos offset))))

(defun ewe-find-right-delimiter (&optional pos offset)
  "Find the left (opening) delimiter of a pair at POS (point by default) and OFFSET (0 by default). Returns a position."
  (interactive)
  (or offset
      (setq offset 0))
  (or pos
      (setq pos (+ (point) offset)))
  (let* ((ppos (+ pos 1))
	 (depth (car-safe (syntax-ppss ppos))))
    (if (> depth 0)
	(progn (while (and (>= (car-safe (syntax-ppss ppos)) depth) (< ppos (point-max)))
		 (setq ppos (+ ppos 1)))
	       (- ppos 1)))))

(defun ewe-find-left-quote (&optional pos offset)
  "Find the opening quote and return its position."
  (interactive)
  (or offset
      (setq offset 0))
  (or pos
      (setq pos (+ (point) offset)))
  (if (elt (syntax-ppss pos) 3)
      (progn (let* ((ppos pos)
		    (in-string (elt (syntax-ppss ppos) 3)))
	       (while in-string
		 (setq ppos (- ppos 1))
		 (setq in-string (elt (syntax-ppss ppos) 3)))
	       ppos))
    (progn (minibuffer-message "Not in string.")
	   pos)))

(defun ewe-find-right-quote (&optional pos offset)
  "Find the closing quote and return its position."
  (interactive)
  (or offset
      (setq offset 0))
  (or pos
      (setq pos (+ (point) offset 1)))
  (if (elt (syntax-ppss pos) 3)
      (progn (let* ((ppos pos)
		    (in-string (elt (syntax-ppss ppos) 3)))
	       (while in-string
		 (setq ppos (+ ppos 1))
		 (setq in-string (elt (syntax-ppss ppos) 3)))
	       (- ppos 1)))
    (progn (minibuffer-message "Not in string.")
	   (- pos offset 1))))

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
    (goto-char (ewe-match-regex-after reg (+ (point) 1)))))

(defun ewe-move-back-regex ()
  "Move backward to regex."
  (interactive)
  (let ((reg (read-from-minibuffer "Regex: ")))
    (goto-char (ewe-match-regex-before reg (point)))))

(defun ewe-move-back-delimiter ()
  "Move backward to pair opening."
  (interactive)
  (let ((pos (point))
	(target (ewe-find-left-delimiter)))
    (if (equal pos target)
	(goto-char (scan-lists pos -1 0))
      (goto-char target))))

(defun ewe-move-forward-delimiter ()
  "Move forward to pair closing."
  (interactive)
  (let ((pos (point))
	(target (ewe-find-right-delimiter)))
    (if (or (equal pos target) (equal target nil))
	(goto-char (- (scan-lists (+ pos 1) 1 0) 1))
      (goto-char target))))

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
  (kill-region (point) (ewe-find-letter-forward 1)))

(defun ewe-kill-through-char ()
  "Kill up to and including a character."
  (interactive)
  (kill-region (point) (+ (ewe-find-letter-forward 1) 1)))

(defun ewe-kill-inside-pair ()
  "Kill inside pair."
  (interactive)
  (let ((beg (ewe-find-left-delimiter))
	(end (ewe-find-right-delimiter)))
    (if (or (not beg) (not end))
	(progn  (left-char) (minibuffer-message "Not in pair."))
      (progn (setq beg (+ beg 1))
	     (setq end (- end 1))
	     (kill-region beg end)
	     (left-char 2)))))

(defun ewe-kill-around-pair ()
  "Kill around pair."
  (interactive)
  (let ((beg (ewe-find-left-delimiter))
	(end (ewe-find-right-delimiter)))
    (if (or (not beg) (not end))
	(progn (left-char) (minibuffer-message "Not in pair."))
      (progn (kill-region beg end)
	     (left-char)))))

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
  (kill-ring-save (point) (ewe-find-letter-forward 1)))

(defun ewe-yank-through-char ()
  "Yank up to and including char."
  (interactive)
  (kill-ring-save (point) (+ (ewe-find-letter-forward 1) 1)))

(defun ewe-yank-inside-pair ()
  "Yank inside pair."
  (interactive)
  (let ((pos (point))
	(beg (ewe-find-left-delimiter))
	(end (ewe-find-right-delimiter)))
    (if (not beg)
	(minibuffer-message "Not in pair")
      (or end
	  (setq end (point)))
      (setq beg (+ beg 1))
      (setq end (- end 1))
      (kill-ring-save beg end)
      (goto-char pos))))

(defun ewe-yank-around-pair ()
  "Yank around pair."
  (interactive)
  (let ((pos (point))
	(beg (ewe-find-left-delimiter))
	(end (ewe-find-right-delimiter)))
    (if (or (equal beg nil) (equal end nil))
	(minibuffer-message "Not in pair.")
      (kill-ring-save beg end)
      (goto-char pos))))


;; Minor Minor Modes
;;; Regex Mode
(defvar ewe-regex-mode-map (make-keymap) "Keymap for EWE-Regex matching.")
(defvar ewe-current-buffer nil "Current buffer reference.")
(defvar-local ewe-regex-current nil "Current regex to search for.")

(define-minor-mode ewe-regex-match-mode
  "EWE Regex Matching mode."
  :lighter " EWE-reg"
  :keymap ewe-regex-mode-map
  (setq ewe-current-buffer (current-buffer))
  (ewe-regex-prompt))

;;;; Functions
;;;;; Utility
(defun ewe-get-regex-string (regex string)
  "Find the length of REGEX in STRING, where string starts with REGEX."
  (let* ((end 0)
	 (pos 0)
	 (length 0)
	 (str (substring string 0 end)))
    (while (not (setq pos (string-match regex str)))
      (setq end (+ end 1))
      (setq str (substring string 0 end)))
    str))

(defun ewe-regex-enumerate-matches (regex)
  "Search buffer for REGEX. Returns a list with the format ((MATCHED-STRING . POS))"
  (let ((matches)
	(str (buffer-substring (point-min) (point-max)))
	(start (point-min))
	(pos (point-min)))
    (while (setq pos (string-match regex str start))
      (setq matches (cons `(,pos . ,(ewe-get-regex-string regex (substring str pos (length str)))) matches))
      (setq start (+ 1 pos)))
    matches))

(defun ewe-regex-prompt ()
  "Prompt user for regex."
  (minibuffer-with-setup-hook (lambda () (add-hook 'after-change-functions #'ewe-regex-update-search))
    (read-regexp "Regex: ")))

(defun ewe-regex-update-search (beg end len)
  "Update the regex to search for."
  (if ewe-regex-current
      (save-window-excursion
	(switch-to-buffer ewe-current-buffer)
	(unhighlight-regexp ewe-regex-current)))
  (setq ewe-regex-current (minibuffer-contents))
  (if (not (string-equal "" ewe-regex-current))
      (save-window-excursion
	(switch-to-buffer ewe-current-buffer)
	(highlight-regexp ewe-regex-current 'isearch-face))))

;;;;;; Movement
(defun ewe-regex-mode-move-next ()
  "Move to next regex match."
  (interactive)
  (goto-char (ewe-match-regex-after ewe-regex-current (point))))

;;;; Keybinds
(define-key ewe-regex-mode-map (kbd "RET") 'ewe-regex-mode-move-next)


;; Mode
(define-minor-mode ewe-minor-mode
  "Toggle EWE Minor Mode."
  :lighter " EWE"
  :keymap ewe-mode-map)

(define-globalized-minor-mode ewe-global-mode ewe-minor-mode ewe-minor-mode)

(provide 'ewe-mode)
;;; ewe-mode.el ends here
