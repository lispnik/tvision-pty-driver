;;;; driver.lisp --- launch a TUI binary in a pty, drive it, assert on the screen.
;;;;
;;;; Uses SBCL's RUN-PROGRAM :PTY (no CFFI).  The app sizes itself from LINES /
;;;; COLUMNS (it runs `stty size`, which reports 0 on the fresh pty, so it falls
;;;; back to the env we set).  HOME is redirected to a temp dir so a TUI that
;;;; persists desktop state (~/.tv2-desktop) can't leak between runs.
;;;;
;;;; The core idea for reliable tests: WAIT-FOR polls the screen until the
;;;; expected text appears (or a timeout), instead of guessing fixed sleeps.

(in-package #:tvision-pty-driver)

(defclass driver ()
  ((process  :initarg :process  :accessor driver-process)
   (stream   :initarg :stream   :accessor driver-stream)
   (terminal :initarg :terminal :accessor driver-terminal)
   (checks   :initform '()      :accessor driver-checks)   ; (name . ok) newest-first
   (verbose  :initarg :verbose  :initform t :accessor driver-verbose)))

(defun launch (binary &key (cols 100) (rows 30) args (home (%temp-home)) extra-env)
  "Start BINARY in a pty at COLS x ROWS and return a DRIVER.  HOME is an isolated
temp dir; EXTRA-ENV is a list of \"VAR=val\" strings added to the environment."
  (let* ((env (append (list (format nil "TERM=xterm-256color")
                            (format nil "COLUMNS=~d" cols) (format nil "LINES=~d" rows)
                            (format nil "HOME=~a" home))
                      extra-env
                      (remove-if (lambda (e) (or (%env-prefix-p e "TERM=") (%env-prefix-p e "COLUMNS=")
                                                 (%env-prefix-p e "LINES=") (%env-prefix-p e "HOME=")))
                                 (sb-ext:posix-environ))))
         (p (sb-ext:run-program binary args :pty t :wait nil :environment env
                                            :external-format :utf-8)))
    (make-instance 'driver :process p :stream (sb-ext:process-pty p)
                           :terminal (make-terminal cols rows))))

(defun %env-prefix-p (entry prefix)
  (and (>= (length entry) (length prefix)) (string= entry prefix :end1 (length prefix))))

(defun %temp-home ()
  (let ((dir (format nil "/tmp/tpd-home-~36r/" (get-internal-real-time))))
    (ensure-directories-exist dir) dir))

;;; --- reading (feed the terminal) --------------------------------------------

(defun drain (drv seconds)
  "Read whatever the app has emitted for SECONDS, feeding the terminal emulator."
  (let ((end (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second))))
        (s (driver-stream drv)) (tm (driver-terminal drv)))
    (loop while (< (get-internal-real-time) end)
          do (let ((c (handler-case (read-char-no-hang s nil :eof) (error () :eof))))
               (cond ((null c) (sleep 0.005))          ; nothing available yet
                     ((eq c :eof) (return))
                     (t (terminal-feed-char tm c)))))))

(defun screen-text (drv) (terminal-text (driver-terminal drv)))
(defun line (drv y) (terminal-line (driver-terminal drv) y))
(defun found? (drv substr) (and (nth-value 1 (terminal-find (driver-terminal drv) substr)) t))
(defun find-text (drv substr) (terminal-find (driver-terminal drv) substr))

(defun wait-for (drv substr &key (timeout 8))
  "Poll until SUBSTR appears on screen (feeding output as it arrives).  Returns T
on success, NIL on timeout.  This is what makes the tests robust."
  (let ((end (+ (get-internal-real-time) (round (* timeout internal-time-units-per-second)))))
    (loop
      (drain drv 0.05)
      (when (found? drv substr) (return t))
      (when (>= (get-internal-real-time) end) (return nil)))))

(defun wait-gone (drv substr &key (timeout 8))
  "Poll until SUBSTR is no longer on screen."
  (let ((end (+ (get-internal-real-time) (round (* timeout internal-time-units-per-second)))))
    (loop
      (drain drv 0.05)
      (unless (found? drv substr) (return t))
      (when (>= (get-internal-real-time) end) (return nil)))))

;;; --- sending input ----------------------------------------------------------

(defun send (drv bytes &optional (settle 0.05))
  "Write BYTES (a string) to the app, then let it react for SETTLE seconds."
  (write-string bytes (driver-stream drv))
  (finish-output (driver-stream drv))
  (drain drv settle))

(defun type-text (drv text &key (cps 60))
  "Type TEXT one character at a time (so an editor's paren-matcher keeps up)."
  (loop for ch across text do (send drv (string ch) (/ 1.0 cps))))

(defparameter +keys+
  `(("enter" . ,(string #\Return)) ("return" . ,(string #\Return))
    ("esc" . ,(string #\Escape))   ("tab" . ,(string #\Tab))
    ("bs" . ,(string #\Rubout))    ("space" . " ")
    ("up" . ,(coerce '(#\Escape #\[ #\A) 'string)) ("down" . ,(coerce '(#\Escape #\[ #\B) 'string))
    ("right" . ,(coerce '(#\Escape #\[ #\C) 'string)) ("left" . ,(coerce '(#\Escape #\[ #\D) 'string))
    ("home" . ,(coerce '(#\Escape #\[ #\H) 'string)) ("end" . ,(coerce '(#\Escape #\[ #\F) 'string))
    ("pgup" . ,(coerce '(#\Escape #\[ #\5 #\~) 'string)) ("pgdn" . ,(coerce '(#\Escape #\[ #\6 #\~) 'string))
    ("del" . ,(coerce '(#\Escape #\[ #\3 #\~) 'string)) ("ins" . ,(coerce '(#\Escape #\[ #\2 #\~) 'string))
    ("s-right" . ,(coerce '(#\Escape #\[ #\1 #\; #\2 #\C) 'string))
    ("s-left"  . ,(coerce '(#\Escape #\[ #\1 #\; #\2 #\D) 'string))
    ("s-up"    . ,(coerce '(#\Escape #\[ #\1 #\; #\2 #\A) 'string))
    ("s-down"  . ,(coerce '(#\Escape #\[ #\1 #\; #\2 #\B) 'string))
    ;; function keys (xterm) + a few modified variants used for menu accelerators
    ("f1" . ,(coerce '(#\Escape #\[ #\1 #\1 #\~) 'string)) ("f2" . ,(coerce '(#\Escape #\[ #\1 #\2 #\~) 'string))
    ("f3" . ,(coerce '(#\Escape #\[ #\1 #\3 #\~) 'string)) ("f4" . ,(coerce '(#\Escape #\[ #\1 #\4 #\~) 'string))
    ("f5" . ,(coerce '(#\Escape #\[ #\1 #\5 #\~) 'string)) ("f6" . ,(coerce '(#\Escape #\[ #\1 #\7 #\~) 'string))
    ("c-f5" . ,(coerce '(#\Escape #\[ #\1 #\5 #\; #\5 #\~) 'string))    ; Ctrl-F5
    ("s-f6" . ,(coerce '(#\Escape #\[ #\1 #\7 #\; #\2 #\~) 'string))    ; Shift-F6
    ("a-f3" . ,(coerce '(#\Escape #\[ #\1 #\; #\3 #\R) 'string)))       ; Alt-F3
  "Named keys -> the bytes a terminal sends.")

(defun key (drv name &key (times 1) (settle 0.08))
  "Send a named key (see +KEYS+) TIMES.  NAME is case-insensitive."
  (let ((bytes (cdr (assoc (string-downcase name) +keys+ :test #'string=))))
    (unless bytes (error "unknown key: ~a" name))
    (dotimes (i times) (send drv bytes settle))))

(defun ctrl (drv ch &optional (settle 0.08))
  "Send Ctrl-CH (e.g. (ctrl drv #\\r) for Ctrl-R)."
  (send drv (string (code-char (logand (char-code (char-upcase ch)) #x1f))) settle))

(defun alt (drv ch &optional (settle 0.12))
  "Send Alt-CH (ESC prefix), e.g. (alt drv #\\l) to open a menu whose hotkey is L."
  (send drv (coerce (list #\Escape (char-downcase ch)) 'string) settle))

;;; --- mouse (SGR 1006, 1-based coordinates) ----------------------------------

(defun click (drv col row &optional (settle 0.12))
  "Left-click at 0-based (COL, ROW): SGR press then release."
  (send drv (format nil "~c[<0;~d;~dM" #\Escape (1+ col) (1+ row)) 0.02)
  (send drv (format nil "~c[<0;~d;~dm" #\Escape (1+ col) (1+ row)) settle))

(defun click-text (drv substr &key (dx 0) (timeout 6) (settle 0.12))
  "Wait for SUBSTR, then click it (offset DX columns into it).  Returns T/NIL."
  (when (wait-for drv substr :timeout timeout)
    (multiple-value-bind (col row) (find-text drv substr)
      (when col (click drv (+ col dx) row settle) t))))

;;; --- menu helpers -----------------------------------------------------------

(defun open-menu (drv hotkey &key (settle 0.25))
  "Open the top-level menu whose hotkey letter is HOTKEY (Alt-<letter>)."
  (alt drv hotkey settle))

(defun menu-item (drv label &key (timeout 4))
  "Click a (visible) menu item / submenu parent by its LABEL.  Chain calls to walk
into submenus: (open-menu d #\\l) (menu-item d \"Debug / trace\") (menu-item d \"Call tree\")."
  (click-text drv label :timeout timeout))

;;; --- assertions -------------------------------------------------------------

(defun check (drv name ok)
  (push (cons name (and ok t)) (driver-checks drv))
  (when (driver-verbose drv)
    (format t "  ~a ~a~%" (if ok "ok  " "FAIL") name))
  (and ok t))

(defun driver-failures (drv)
  (count-if-not #'cdr (driver-checks drv)))

(defun report (drv &key (title "pty smoke"))
  "Print a summary; return 0 when all checks passed, else 1."
  (let* ((all (reverse (driver-checks drv)))
         (n (length all)) (fails (driver-failures drv)))
    (format t "~&~a: ~d/~d passed~@[  (~d FAILED)~]~%" title (- n fails) n (and (plusp fails) fails))
    (if (zerop fails) 0 1)))

;;; --- lifecycle --------------------------------------------------------------

(defun quit-driver (drv)
  (ignore-errors (send drv (string (code-char 17)) 0.1))   ; try a clean Ctrl-Q first
  (ignore-errors (sb-ext:process-kill (driver-process drv) 9))
  (ignore-errors (sb-ext:process-wait (driver-process drv)))
  (ignore-errors (close (driver-stream drv))))

(defmacro with-driver ((var binary &rest args) &body body)
  `(let ((,var (launch ,binary ,@args)))
     (unwind-protect (progn ,@body) (quit-driver ,var))))
