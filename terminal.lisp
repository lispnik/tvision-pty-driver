;;;; terminal.lisp --- a small VT/ANSI screen emulator.
;;;;
;;;; Enough of an xterm to reconstruct what a TUI painted: absolute cursor moves
;;;; (CSI H/f), erases (J/K), CR/LF, and printable text placed into a fixed grid
;;;; with East-Asian *display width* (a wide glyph occupies two cells).  Colours
;;;; (SGR) and private modes (?1049h, ?25l, mouse) are parsed and ignored.  Fed
;;;; one character at a time, so it works on a streaming pty read.

(in-package #:revision-pty-driver)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (require :sb-unicode)))

(declaim (inline char-width))
(defun char-width (ch)
  "Display columns for CH: 2 for East-Asian wide/fullwidth glyphs, else 1."
  (let ((code (char-code ch)))
    (if (< code #x1100) 1
        (case (ignore-errors (sb-unicode:east-asian-width ch)) ((:w :f) 2) (t 1)))))

(defstruct (terminal (:constructor %make-terminal))
  (cols 100 :type fixnum)
  (rows 30  :type fixnum)
  (grid nil)                         ; (simple-array character (rows cols))
  (cx 0 :type fixnum)
  (cy 0 :type fixnum)
  (state :ground)                    ; :ground | :esc | :csi
  (params (make-string-output-stream)))

(defun make-terminal (cols rows)
  (let ((g (make-array (list rows cols) :element-type 'character :initial-element #\Space)))
    (%make-terminal :cols cols :rows rows :grid g)))

(defun %clear (tm)
  (let ((g (terminal-grid tm)))
    (dotimes (y (terminal-rows tm))
      (dotimes (x (terminal-cols tm)) (setf (aref g y x) #\Space)))))

(defconstant +wide-cont+ (code-char 0)
  "Sentinel in a wide glyph's second cell; skipped when reading rows back so a
2-cell glyph reconstructs as one character (日 not \"日 \").")

(defun %put (tm ch)
  (let ((y (terminal-cy tm)) (x (terminal-cx tm)) (w (char-width ch)))
    (when (and (< -1 y (terminal-rows tm)) (< -1 x (terminal-cols tm)))
      (setf (aref (terminal-grid tm) y x) ch)
      (when (and (= w 2) (< (1+ x) (terminal-cols tm)))
        (setf (aref (terminal-grid tm) y (1+ x)) +wide-cont+)))
    (incf (terminal-cx tm) w)))

(defun %csi-nums (s)
  "Parse the numeric parameters of a CSI string like \"12;5\" into a list."
  (let ((clean (remove-if-not (lambda (c) (or (digit-char-p c) (char= c #\;))) s)))
    (loop for tok in (%split clean #\;)
          collect (or (parse-integer tok :junk-allowed t) 0))))

(defun %split (s ch)
  (let ((out '()) (start 0))
    (loop for i from 0 below (length s)
          when (char= (char s i) ch) do (push (subseq s start i) out) (setf start (1+ i)))
    (push (subseq s start) out)
    (nreverse out)))

(defun %csi (tm final params)
  (case final
    ((#\H #\f)                                     ; cursor position (1-based; default 1;1)
     (let ((n (%csi-nums params)))
       (setf (terminal-cy tm) (max 0 (1- (if (>= (length n) 1) (max 1 (first n)) 1)))
             (terminal-cx tm) (max 0 (1- (if (>= (length n) 2) (max 1 (second n)) 1))))))
    (#\J (%clear tm))                              ; erase display (treat any variant as full clear)
    (#\K (let ((g (terminal-grid tm)) (y (terminal-cy tm)))   ; erase line from the cursor to EOL
           (when (< -1 y (terminal-rows tm))
             (loop for x from (max 0 (terminal-cx tm)) below (terminal-cols tm)
                   do (setf (aref g y x) #\Space)))))
    (#\A (decf (terminal-cy tm) (max 1 (or (first (%csi-nums params)) 1))))
    (#\B (incf (terminal-cy tm) (max 1 (or (first (%csi-nums params)) 1))))
    (#\C (incf (terminal-cx tm) (max 1 (or (first (%csi-nums params)) 1))))
    (#\D (decf (terminal-cx tm) (max 1 (or (first (%csi-nums params)) 1))))
    (t nil)))                                      ; m (SGR), h/l (modes), etc.: ignore

(defun terminal-feed-char (tm ch)
  (ecase (terminal-state tm)
    (:ground
     (cond
       ((char= ch #\Escape) (setf (terminal-state tm) :esc))
       ((char= ch #\Return) (setf (terminal-cx tm) 0))
       ((char= ch #\Linefeed) (setf (terminal-cy tm) (min (1- (terminal-rows tm)) (1+ (terminal-cy tm)))))
       ((char= ch #\Backspace) (setf (terminal-cx tm) (max 0 (1- (terminal-cx tm)))))
       ((>= (char-code ch) 32) (%put tm ch))
       (t nil)))
    (:esc
     (setf (terminal-state tm)
           (if (char= ch #\[) (progn (get-output-stream-string (terminal-params tm)) :csi) :ground)))
    (:csi
     (if (<= #x40 (char-code ch) #x7e)             ; a final byte ends the CSI
         (progn (%csi tm ch (get-output-stream-string (terminal-params tm)))
                (setf (terminal-state tm) :ground))
         (write-char ch (terminal-params tm))))))

(defun terminal-feed (tm string)
  (loop for ch across string do (terminal-feed-char tm ch)))

(defun %row-cells (tm y)
  "List of (CHAR . SCREEN-COL) for row Y's real cells (wide-glyph continuation
cells skipped), so text search sees logical characters while column lookups still
report the on-screen position."
  (loop for x below (terminal-cols tm)
        for c = (aref (terminal-grid tm) y x)
        unless (char= c +wide-cont+) collect (cons c x)))

(defun terminal-line (tm y)
  "Row Y as a string (trailing blanks trimmed)."
  (if (< -1 y (terminal-rows tm))
      (string-right-trim " " (coerce (mapcar #'car (%row-cells tm y)) 'string))
      ""))

(defun terminal-text (tm)
  "The whole screen as text, one row per line."
  (format nil "~{~a~^~%~}" (loop for y below (terminal-rows tm) collect (terminal-line tm y))))

(defun terminal-find (tm substr)
  "Return (values SCREEN-COL ROW) of the first row containing SUBSTR, or NIL.
COL is the on-screen column of the match (accounting for wide glyphs)."
  (loop for y below (terminal-rows tm)
        for cells = (%row-cells tm y)
        for idx = (search substr (coerce (mapcar #'car cells) 'string))
        when idx return (values (cdr (nth idx cells)) y)))
