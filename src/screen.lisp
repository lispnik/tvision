;;;; screen.lisp --- The terminal driver: raw mode, ANSI rendering and input.
;;;;
;;;; This is the platform layer that stands in for Turbo Vision's THardwareInfo
;;;; / TScreen.  It keeps an off-screen buffer of cells, and `flush-screen'
;;;; paints only the cells that changed since the previous frame.  Input is
;;;; read non-blocking from fd 0 and decoded into TEvent records.

(in-package #:tvision)

(defstruct (screen (:conc-name screen-))
  (width 80 :type fixnum)
  (height 25 :type fixnum)
  ;; back buffer = what we want on screen; front buffer = what is on screen
  (back  (make-array 0 :element-type '(unsigned-byte 53)) :type (simple-array (unsigned-byte 53) (*)))
  (front (make-array 0 :element-type '(unsigned-byte 53)) :type (simple-array (unsigned-byte 53) (*)))
  (out nil)
  (saved-stty nil)
  (cursor-x 0 :type fixnum)
  (cursor-y 0 :type fixnum)
  (cursor-visible nil)
  ;; raw input bytes awaiting decode, and decoded events awaiting delivery
  (in-buf (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (event-queue '())
  ;; mouse state for double-click detection and auto-repeat synthesis
  (mouse-buttons 0 :type fixnum)
  (mouse-x 0 :type fixnum)
  (mouse-y 0 :type fixnum)
  (last-click-time 0 :type fixnum)
  (last-click-x -1 :type fixnum)
  (last-click-y -1 :type fixnum)
  (click-count 0 :type fixnum)
  (last-auto-time 0 :type fixnum)
  (cursor-shape :underline))

(defvar *screen* nil "The active terminal screen, or NIL when not initialised.")

;;; ---------------------------------------------------------------------------
;;; Low-level terminal helpers
;;; ---------------------------------------------------------------------------

(defun uiop-split (string)
  "Split STRING on runs of whitespace (avoids a UIOP dependency)."
  (let ((tokens '()) (start nil))
    (loop for i from 0 below (length string)
          for ch = (char string i)
          do (if (member ch '(#\Space #\Tab #\Newline #\Return))
                 (when start (push (subseq string start i) tokens) (setf start nil))
                 (unless start (setf start i)))
          finally (when start (push (subseq string start) tokens)))
    (nreverse tokens)))

(defun %stty (&rest args)
  "Run stty against the controlling terminal, returning its trimmed output."
  (with-output-to-string (out)
    (sb-ext:run-program "stty" args :search t :input t :output out)))

(defun %env-int (name)
  (let ((v (sb-ext:posix-getenv name)))
    (and v (parse-integer v :junk-allowed t))))

(defun %query-size ()
  "Return (values rows cols) for the controlling terminal, falling back to the
LINES/COLUMNS environment variables and finally to a sane 24x80 default."
  (let* ((s (handler-case (%stty "size") (error () "")))
         (parts (loop for tok in (uiop-split s)
                      collect (or (parse-integer tok :junk-allowed t) 0))))
    (if (and (>= (length parts) 2) (>= (first parts) 2) (>= (second parts) 2))
        (values (first parts) (second parts))
        (values (or (and (%env-int "LINES") (>= (%env-int "LINES") 2) (%env-int "LINES")) 24)
                (or (and (%env-int "COLUMNS") (>= (%env-int "COLUMNS") 2) (%env-int "COLUMNS")) 80)))))

(defun %emit (s string) (write-string string (screen-out s)))

(defun %flush-out (s) (finish-output (screen-out s)))

;;; xterm control sequences
(defparameter +esc+ (string #\Escape))
(defun ctl (fmt &rest args) (concatenate 'string +esc+ "[" (apply #'format nil fmt args)))

;;; ---------------------------------------------------------------------------
;;; Initialisation / teardown
;;; ---------------------------------------------------------------------------

(defun init-screen ()
  "Put the terminal into raw, full-screen mode and return the screen object."
  (setf *color-mode* (detect-color-mode))   ; pick 24-bit / 256 / 16 for this terminal
  (let ((s (make-screen)))
    (setf (screen-out s)
          (sb-sys:make-fd-stream 1 :output t :element-type 'character
                                   :external-format :utf-8 :buffering :full))
    ;; remember current terminal settings, then go raw
    (setf (screen-saved-stty s) (handler-case (%stty "-g") (error () nil)))
    (handler-case (%stty "raw" "-echo") (error () nil))
    (multiple-value-bind (rows cols) (%query-size)
      (screen-resize s cols rows))
    (%emit s (ctl "?1049h"))      ; enter alternate screen
    (%emit s (ctl "?25l"))        ; hide cursor
    (%emit s (ctl "?1000h"))      ; enable mouse button tracking
    (%emit s (ctl "?1002h"))      ; enable drag tracking
    (%emit s (ctl "?1006h"))      ; SGR extended mouse coordinates
    (%emit s (ctl "2J"))          ; clear screen
    (%flush-out s)
    (setf *screen* s)
    s))

(defun done-screen (&optional (s *screen*))
  "Restore the terminal to its original state."
  (when s
    (%emit s (ctl "?1006l"))
    (%emit s (ctl "?1002l"))
    (%emit s (ctl "?1000l"))
    (%emit s (ctl "?25h"))        ; show cursor
    (%emit s "\e[0m")             ; reset attributes
    (%emit s (ctl "?1049l"))      ; leave alternate screen
    (%flush-out s)
    (when (screen-saved-stty s)
      (handler-case (%stty (screen-saved-stty s)) (error () nil)))
    (handler-case (%stty "sane") (error () nil))
    (when (eq s *screen*) (setf *screen* nil))))

(defmacro with-screen ((&optional var) &body body)
  "Run BODY with an initialised screen, guaranteeing teardown on exit."
  (let ((g (or var (gensym))))
    `(let ((,g (init-screen)))
       (declare (ignorable ,g))
       (unwind-protect (progn ,@body)
         (done-screen ,g)))))

(defun screen-resize (s cols rows)
  (setf (screen-width s) cols
        (screen-height s) rows)
  (let ((n (* cols rows))
        (blank (cell-make-code 32 #x07)))
    (setf (screen-back s)
          (make-array n :element-type '(unsigned-byte 53) :initial-element blank))
    ;; front initialised to an impossible value so the first flush paints all
    (setf (screen-front s)
          (make-array n :element-type '(unsigned-byte 53) :initial-element +impossible-cell+)))
  s)

(defun screen-invalidate (&optional (s *screen*))
  "Force the next FLUSH-SCREEN to repaint every cell (used after a colour-theme
change, where the cells are unchanged but their rendering is not)."
  (when s (fill (screen-front s) +impossible-cell+)))

;;; ---------------------------------------------------------------------------
;;; Drawing into the back buffer
;;; ---------------------------------------------------------------------------

(declaim (inline screen-index))
(defun screen-index (s x y) (+ x (* y (screen-width s))))

(defun screen-cell-set (s x y cell)
  "Set the back-buffer cell at (X,Y).  Coordinates outside the screen are
silently ignored, which lets views draw without bounds-checking."
  (when (and (>= x 0) (< x (screen-width s)) (>= y 0) (< y (screen-height s)))
    (setf (aref (screen-back s) (screen-index s x y)) cell)))

(defun screen-back-buffer (s) (screen-back s))

;;; ---------------------------------------------------------------------------
;;; Flushing: diff the back buffer against the front buffer and paint.
;;; ---------------------------------------------------------------------------

(defun flush-screen (&optional (s *screen*))
  (let* ((back (screen-back s))
         (front (screen-front s))
         (w (screen-width s))
         (h (screen-height s))
         (out (screen-out s))
         (last-attr -1)
         (cx -1) (cy -1))            ; last cursor position written
    (%emit s (ctl "?25l"))
    (dotimes (y h)
      (dotimes (x w)
        (let* ((idx (+ x (* y w)))
               (cell (aref back idx)))
          (when (/= cell (aref front idx))
            (setf (aref front idx) cell)
            ;; reposition the cursor if we are not already there
            (unless (and (= y cy) (= x cx))
              (write-string (ctl "~d;~dH" (1+ y) (1+ x)) out)
              (setf cy y cx x))
            (let ((attr (cell-attr cell)))
              (unless (= attr last-attr)
                (write-string (attr->ansi attr) out)
                (setf last-attr attr)))
            (let ((code (cell-char-code cell)))
              (write-char (if (< code 32) #\Space (code-char code)) out))
            (incf cx)))))
    ;; place the hardware cursor where a focused view asked for it
    (when (screen-cursor-visible s)
      (write-string (ctl "~d q" (ecase (screen-cursor-shape s)
                                  (:block 1) (:underline 3) (:bar 5))) out)
      (write-string (ctl "~d;~dH" (1+ (screen-cursor-y s)) (1+ (screen-cursor-x s))) out)
      (write-string (ctl "?25h") out))
    (%flush-out s)))

(defun set-cursor-pos (s x y)
  (setf (screen-cursor-x s) x (screen-cursor-y s) y))
(defun set-cursor-shape (shape &optional (s *screen*))
  "SHAPE is :block, :underline, or :bar."
  (when s (setf (screen-cursor-shape s) shape)))
(defun show-cursor (&optional (s *screen*)) (setf (screen-cursor-visible s) t))
(defun hide-cursor (&optional (s *screen*)) (setf (screen-cursor-visible s) nil))

;;; ---------------------------------------------------------------------------
;;; Input: non-blocking byte reads decoded into TEvents.
;;; ---------------------------------------------------------------------------

(defun %read-available (s)
  "Read whatever bytes are ready on fd 0 into the screen's input buffer."
  (let ((tmp (make-array 512 :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (tmp)
      (loop
        (multiple-value-bind (n err)
            (ignore-errors (sb-unix:unix-read 0 (sb-sys:vector-sap tmp) 512))
          (declare (ignore err))
          (when (or (null n) (<= n 0)) (return))
          (dotimes (i n) (vector-push-extend (aref tmp i) (screen-in-buf s)))
          (when (< n 512) (return)))))))

(defparameter +auto-repeat-ticks+
  (max 1 (round (* 0.10 internal-time-units-per-second)))
  "Interval between synthesized ev-mouse-auto events while a button is held.")
(defvar *double-click-ticks*
  (max 1 (round (* 0.40 internal-time-units-per-second)))
  "Maximum gap between successive clicks to count as a multi-click.")

(defun set-double-click-time (seconds)
  "Configure the maximum gap (in seconds) for double/triple-click detection."
  (setf *double-click-ticks* (max 1 (round (* seconds internal-time-units-per-second)))))

(defvar *input-multiplexer* nil
  "When set (by the concurrency layer), a function (TIMEOUT) -> :FD0 | NIL that
waits for terminal input OR a worker-thread wakeup, so background evaluation can
wake the UI loop instantly.  When NIL, PUMP-INPUT just polls fd 0 directly.")

(defun %auto-repeat (s)
  "No new input, but a mouse button is held -> synthesize ev-mouse-auto."
  (when (plusp (screen-mouse-buttons s))
    (let ((now (get-internal-real-time)))
      (when (>= (- now (screen-last-auto-time s)) +auto-repeat-ticks+)
        (setf (screen-last-auto-time s) now)
        (setf (screen-event-queue s)
              (nconc (screen-event-queue s)
                     (list (make-event :type +ev-mouse-auto+
                                       :mouse-buttons (screen-mouse-buttons s)
                                       :mouse-where (make-tpoint (screen-mouse-x s)
                                                                 (screen-mouse-y s))))))))))

(defun pump-input (s timeout)
  "Wait up to TIMEOUT seconds for input, decode it, and queue events.  If a
mouse button is held with nothing else pending, synthesize ev-mouse-auto."
  (let ((ready (if *input-multiplexer*
                   (funcall *input-multiplexer* timeout)
                   (and (sb-sys:wait-until-fd-usable 0 :input timeout nil) :fd0))))
    (cond
      ((eq ready :fd0) (%read-available s) (decode-input s))
      (t (%auto-repeat s)))))

(defun %update-mouse-state (s e)
  "Track button state for auto-repeat, and tag double-clicks."
  (let ((ty (event-type e)) (p (event-mouse-where e)))
    (cond
      ((= ty +ev-mouse-down+)
       (setf (screen-mouse-buttons s) (event-mouse-buttons e)
             (screen-mouse-x s) (point-x p) (screen-mouse-y s) (point-y p))
       (let ((now (get-internal-real-time)))
         (setf (screen-last-auto-time s) now)
         (if (and (<= (- now (screen-last-click-time s)) *double-click-ticks*)
                  (= (point-x p) (screen-last-click-x s))
                  (= (point-y p) (screen-last-click-y s)))
             (incf (screen-click-count s))
             (setf (screen-click-count s) 1))
         (when (>= (screen-click-count s) 2) (setf (event-double e) t))
         (when (>= (screen-click-count s) 3) (setf (event-triple e) t))
         (setf (screen-last-click-time s) now
               (screen-last-click-x s) (point-x p)
               (screen-last-click-y s) (point-y p))))
      ((= ty +ev-mouse-up+)
       (setf (screen-mouse-buttons s) 0))
      ((= ty +ev-mouse-move+)
       (setf (screen-mouse-x s) (point-x p) (screen-mouse-y s) (point-y p))))))

(defun decode-input (s)
  (let ((buf (screen-in-buf s)))
    (multiple-value-bind (events consumed) (parse-input-buffer buf (fill-pointer buf))
      (dolist (e events)
        (%update-mouse-state s e)
        (setf (screen-event-queue s) (nconc (screen-event-queue s) (list e))))
      ;; shift unconsumed bytes (a partial escape sequence) to the front
      (when (> consumed 0)
        (let ((remaining (- (fill-pointer buf) consumed)))
          (dotimes (i remaining) (setf (aref buf i) (aref buf (+ i consumed))))
          (setf (fill-pointer buf) remaining))))))

(defun screen-next-event (s)
  "Pop the next decoded event, or NIL if none are pending."
  (when (screen-event-queue s)
    (pop (screen-event-queue s))))

;;; --- escape-sequence decoder ----------------------------------------------

(defun key-event (key-code &optional (char-code 0))
  (make-event :type +ev-key-down+ :key-code key-code :char-code char-code))

(defun parse-plain-byte (b)
  (cond
    ((= b 13) (key-event +kb-enter+ 13))
    ((= b 10) (key-event +kb-enter+ 13))
    ((= b 9)  (key-event +kb-tab+ 9))
    ((or (= b 8) (= b 127)) (key-event +kb-back+ 8))
    ((and (>= b 1) (<= b 26))              ; Ctrl-A .. Ctrl-Z
     (make-event :type +ev-key-down+ :key-code b :char-code b :modifiers +md-ctrl+))
    ((< b 32) (key-event b b))             ; other control chars
    (t (key-event b b))))                  ; printable

(defun %utf8-seq-len (lead)
  "Expected total byte length of a UTF-8 sequence from its LEAD byte
(1 for ASCII or a stray continuation byte)."
  (cond ((< lead #x80) 1) ((< lead #xc0) 1)      ; ASCII / stray continuation
        ((< lead #xe0) 2) ((< lead #xf0) 3) ((< lead #xf8) 4) (t 1)))

(defun parse-utf8 (buf i len)
  "Decode the UTF-8 sequence at BUF[I] (lead byte >= #x80) into one key event.
Return (values event consumed), or (values nil nil) if the sequence is not yet
complete in the buffer (wait for more bytes)."
  (let* ((lead (aref buf i)) (n (%utf8-seq-len lead)))
    (cond
      ((= n 1) (values (key-event lead lead) 1))   ; stray continuation byte
      ((> (+ i n) len) (values nil nil))           ; incomplete -- wait
      (t (let ((cp (logand lead (ecase n (2 #x1f) (3 #x0f) (4 #x07)))))
           (loop for k from 1 below n
                 for b = (aref buf (+ i k))
                 do (if (= (logand b #xc0) #x80)
                        (setf cp (logior (ash cp 6) (logand b #x3f)))
                        (return-from parse-utf8 (values (key-event lead lead) 1)))) ; bad seq
           (if (and (<= cp #x10ffff) (>= cp #x80))
               (values (key-event cp cp) n)
               (values (key-event lead lead) 1)))))))

(defun parse-input-buffer (buf len)
  "Decode BUF[0,LEN) into a list of events.  Return (values events consumed),
leaving any trailing partial escape / UTF-8 sequence for the next read."
  (let ((events '()) (i 0))
    (loop while (< i len) do
      (let ((b (aref buf i)))
        (cond
          ((= b 27)
           (multiple-value-bind (ev consumed) (parse-escape buf i len)
             (if (null consumed)
                 (return)                  ; incomplete; wait for more bytes
                 (progn (when ev (push ev events)) (incf i consumed)))))
          ((>= b #x80)                      ; UTF-8 lead/continuation byte
           (multiple-value-bind (ev consumed) (parse-utf8 buf i len)
             (if (null consumed)
                 (return)                  ; incomplete multi-byte char
                 (progn (when ev (push ev events)) (incf i consumed)))))
          (t (push (parse-plain-byte b) events) (incf i)))))
    (values (nreverse events) i)))

(defun parse-escape (buf i len)
  "Parse an escape sequence beginning at BUF[I].  Return (values event consumed)
or (values nil nil) when more bytes are required."
  (cond
    ;; lone ESC at end of buffer
    ((>= (1+ i) len) (values (key-event +kb-esc+ 27) 1))
    (t (let ((c (aref buf (1+ i))))
         (cond
           ((= c (char-code #\[)) (parse-csi buf i len))
           ((= c (char-code #\O)) (parse-ss3 buf i len))
           ;; ESC x / ESC X  ->  Alt-X (the Quit shortcut, with a stable key-code)
           ((or (= c (char-code #\x)) (= c (char-code #\X)))
            (values (make-event :type +ev-key-down+ :key-code +kb-alt-x+
                                :char-code (char-code #\x) :modifiers +md-alt+)
                    2))
           ;; ESC <printable>  ->  Alt-<char>
           ((<= 32 c 126)
            (values (make-event :type +ev-key-down+ :key-code 0
                                :char-code c :modifiers +md-alt+)
                    2))
           ;; ESC followed by something else: deliver bare ESC, reparse the rest
           (t (values (key-event +kb-esc+ 27) 1)))))))

(defun parse-ss3 (buf i len)
  (if (>= (+ i 2) len)
      (values nil nil)
      (let ((f (aref buf (+ i 2))))
        (values
         (case (code-char f)
           (#\P (key-event +kb-f1+)) (#\Q (key-event +kb-f2+))
           (#\R (key-event +kb-f3+)) (#\S (key-event +kb-f4+))
           (#\A (key-event +kb-up+)) (#\B (key-event +kb-down+))
           (#\C (key-event +kb-right+)) (#\D (key-event +kb-left+))
           (#\H (key-event +kb-home+)) (#\F (key-event +kb-end+))
           (t nil))
         3))))

(defun parse-csi (buf i len)
  "Parse a CSI (ESC [) sequence, including SGR mouse reports."
  ;; find the final byte (0x40-0x7e)
  (let ((j (+ i 2)))
    (loop
      (when (>= j len) (return-from parse-csi (values nil nil)))
      (let ((b (aref buf j)))
        (when (<= #x40 b #x7e) (return))
        (incf j)))
    (let* ((final (aref buf j))
           (params-start (+ i 2))
           (sgr-mouse (and (> j params-start) (= (aref buf params-start) (char-code #\<)))))
      (if sgr-mouse
          (parse-mouse buf (1+ params-start) j final)
          (let* ((nums (parse-csi-numbers buf params-start j))
                 (n (if nums (first nums) 0))
                 ;; xterm encodes modifiers as the 2nd param: 1 + bitmask
                 (mods (if (>= (length nums) 2)
                           (let ((m (1- (second nums))))
                             (logior (if (logtest m 1) +md-shift+ 0)
                                     (if (logtest m 2) +md-alt+ 0)
                                     (if (logtest m 4) +md-ctrl+ 0)))
                           0))
                 (ev (case (code-char final)
                       (#\A (key-event +kb-up+)) (#\B (key-event +kb-down+))
                       (#\C (key-event +kb-right+)) (#\D (key-event +kb-left+))
                       (#\H (key-event +kb-home+)) (#\F (key-event +kb-end+))
                       (#\Z (key-event +kb-shift-tab+))
                       (#\~ (case n
                              ((1 7) (key-event +kb-home+))
                              (2 (key-event +kb-ins+))
                              (3 (key-event +kb-del+))
                              ((4 8) (key-event +kb-end+))
                              (5 (key-event +kb-pgup+))
                              (6 (key-event +kb-pgdn+))
                              (11 (key-event +kb-f1+)) (12 (key-event +kb-f2+))
                              (13 (key-event +kb-f3+)) (14 (key-event +kb-f4+))
                              (15 (key-event +kb-f5+)) (17 (key-event +kb-f6+))
                              (18 (key-event +kb-f7+)) (19 (key-event +kb-f8+))
                              (20 (key-event +kb-f9+)) (21 (key-event +kb-f10+))
                              (t nil)))
                       (t nil))))
            (when (and ev (plusp mods)) (setf (event-modifiers ev) mods))
            (values ev (- (1+ j) i)))))))

(defun parse-csi-numbers (buf start end)
  "Return the list of integers in the `;'-separated parameter run BUF[start,end)."
  (let ((nums '()) (cur nil))
    (loop for k from start below end
          for b = (aref buf k)
          do (cond
               ((<= (char-code #\0) b (char-code #\9))
                (setf cur (+ (* (or cur 0) 10) (- b (char-code #\0)))))
               ((= b (char-code #\;))
                (push (or cur 0) nums) (setf cur nil))))
    (when cur (push cur nums))
    (nreverse nums)))

(defun parse-mouse (buf start final-idx final)
  "Parse an SGR mouse report `<b;x;yM' or `...m', including wheel + modifiers."
  (let* ((nums (parse-csi-numbers buf start final-idx))
         (b (or (first nums) 0))
         (x (1- (or (second nums) 1)))
         (y (1- (or (third nums) 1)))
         (consumed (- (1+ final-idx) (- start 3)))   ; whole "ESC[<...final"
         (release (= final (char-code #\m)))
         (mods (logior (if (logtest b 4) +md-shift+ 0)
                       (if (logtest b 8) +md-alt+ 0)
                       (if (logtest b 16) +md-ctrl+ 0))))
    (if (logtest b 64)
        ;; wheel: low bit 0 = up, 1 = down
        (values (make-event :type +ev-mouse-wheel+ :modifiers mods
                            :mouse-where (make-tpoint x y)
                            :wheel (if (logtest b 1) +mw-down+ +mw-up+))
                consumed)
        (let* ((motion (logtest b 32))
               (button (logand b 3))
               (buttons (cond ((= button 0) +mb-left+)
                              ((= button 2) +mb-right+)
                              (t 0))))
          (values
           (make-event
            :type (cond (release +ev-mouse-up+)
                        (motion +ev-mouse-move+)
                        (t +ev-mouse-down+))
            :modifiers mods
            :mouse-where (make-tpoint x y)
            :mouse-buttons (if release 0 buttons))
           consumed)))))
