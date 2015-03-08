;;; ewe-mode --- Edit With Emotion
;;; Commentary:
;;;;
;;; Code:

;; Basic defines
(defvar ewe-mode-hook nil)

;;; Set up keymap
(defvar ewe-mode-map (make-keymap) "Keymap for EWE major mode.")

;;;; Move Commands
(define-key ewe-mode-map (kbd "C-S-p") 'ewe-move-beginning-of-paragraph)
(define-key ewe-mode-map (kbd "M-p") 'ewe-move-beginning-of-paragraph)
(define-key ewe-mode-map (kbd "C-S-n") 'ewe-move-end-of-paragraph)
(define-key ewe-mode-map (kbd "M-n") 'ewe-move-end-of-paragraph)
(define-key ewe-mode-map (kbd "C-S-f") 'ewe-move-end-of-word)
(define-key ewe-mode-map (kbd "C-S-b") 'ewe-move-beginning-of-word)

;;;; Kill Commands
(define-key ewe-mode-map (kbd "C-k l") 'ewe-kill-line)
(define-key ewe-mode-map (kbd "C-k k") 'ewe-kill-line)
(define-key ewe-mode-map (kbd "C-k C-k") 'ewe-kill-line)
(define-key ewe-mode-map (kbd "C-k p") 'ewe-kill-paragraph)
(define-key ewe-mode-map (kbd "C-k w") 'ewe-kill-word-forward)
(define-key ewe-mode-map (kbd "C-S-d") 'ewe-kill-word-forward)

;;;; Yank Commands
(define-key ewe-mode-map (kbd "M-k l") 'ewe-yank-line)
(define-key ewe-mode-map (kbd "M-k k") 'ewe-yank-line)
(define-key ewe-mode-map (kbd "M-k M-k") 'ewe-yank-line)
(define-key ewe-mode-map (kbd "M-k p") 'ewe-yank-paragraph)

;; Functions
;;; Utility
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
    (while (not (or (member (char-after (- pos chars-back)) '(?  ?\( ?\) ?' ?\\ ?/ ?\" ?. ?, ?\; ?: ?\n ?-)) (equal (- pos chars-back) (point-min))))
      (setq chars-back (+ chars-back 1)))
    (- pos (- chars-back 1))))


(defun ewe-find-end-of-word (&optional offset)
  "Find the end of the word from OFFSET."
  (interactive)
  (if (not offset)
      (setq offset 0))
  (let ((chars-forward offset)
	(pos (point)))
    (while (not (or (member (char-after (+ pos chars-forward)) '(?  ?\( ?\) ?' ?\\ ?/ ?- ?\" ?. ?, ?\; ?: ?\n)) (equal (+ pos chars-forward) (point-min))))
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

(defun ewe-move-beginning-of-word ()
  "Move to the end of word."
  (interactive)
  (goto-char (ewe-find-beginning-of-word 2)))

(defun ewe-move-end-of-word ()
  "Move to the end of word."
  (interactive)
  (goto-char (ewe-find-end-of-word 1)))

;;; Kill
(defun ewe-kill-line ()
  "Kill entire line."
  (interactive)
  (kill-region (line-beginning-position) (line-beginning-position 2)))

(defun ewe-kill-paragraph ()
  "Kill a paragraph."
  (interactive)
  (kill-region (ewe-find-beginning-of-paragraph) (ewe-find-end-of-paragraph)))

(defun ewe-kill-word-forward ()
  "Kill a word."
  (interactive)
  (kill-region (ewe-find-beginning-of-word) (ewe-find-end-of-word)))

;;; Yank
(defun ewe-yank-line ()
  "Yank entire line."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

(defun ewe-yank-paragraph ()
  "Yank entire paragraph."
  (interactive)
  (kill-ring-save (ewe-find-beginning-of-paragraph) (ewe-find-end-of-paragraph)))


;; Mode
(define-minor-mode ewe-minor-mode
  "Toggle EWE Minor Mode."
  :lighter " EWE"
  :keymap ewe-mode-map)

(define-globalized-minor-mode ewe-global-mode ewe-minor-mode ewe-minor-mode)

(provide 'ewe-mode)
;;; ewe-mode.el ends here
