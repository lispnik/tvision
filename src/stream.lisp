;;;; stream.lisp --- Binary streams and resource files (TStream / TResourceFile).
;;;;
;;;; A compact tagged binary encoding for the plist values that EXTERNALIZE
;;;; produces (nil, t, integers, strings, keywords, lists), plus a resource file
;;;; that maps names to stored objects.  Complements the readable S-expression
;;;; persistence in persist.lisp with a true binary format.

(in-package #:tvision)

;;; --- primitive byte I/O ----------------------------------------------------

(defun write-u8 (s b) (write-byte (logand b #xff) s))
(defun read-u8 (s) (read-byte s))

(defun write-u32 (s n)
  (dotimes (i 4) (write-byte (logand (ash n (* -8 i)) #xff) s)))
(defun read-u32 (s)
  (let ((n 0)) (dotimes (i 4) (setf n (logior n (ash (read-byte s) (* 8 i))))) n))

(defun write-i64 (s n)
  (let ((u (logand n #xffffffffffffffff)))
    (dotimes (i 8) (write-byte (logand (ash u (* -8 i)) #xff) s))))
(defun read-i64 (s)
  (let ((u 0))
    (dotimes (i 8) (setf u (logior u (ash (read-byte s) (* 8 i)))))
    (if (>= u #.(expt 2 63)) (- u #.(expt 2 64)) u)))

(defun write-bstring (s str)
  (let ((bytes (sb-ext:string-to-octets str :external-format :utf-8)))
    (write-u32 s (length bytes))
    (write-sequence bytes s)))
(defun read-bstring (s)
  (let* ((len (read-u32 s)) (bytes (make-array len :element-type '(unsigned-byte 8))))
    (read-sequence bytes s)
    (sb-ext:octets-to-string bytes :external-format :utf-8)))

;;; --- tagged value tree -----------------------------------------------------

(defconstant +tag-nil+     0)
(defconstant +tag-t+       1)
(defconstant +tag-int+     2)
(defconstant +tag-string+  3)
(defconstant +tag-keyword+ 4)
(defconstant +tag-list+    5)

(defun stream-write-value (s v)
  "Write a value (nil/t/integer/string/keyword/list) to binary stream S."
  (cond
    ((null v)       (write-u8 s +tag-nil+))
    ((eq v t)       (write-u8 s +tag-t+))
    ((integerp v)   (write-u8 s +tag-int+) (write-i64 s v))
    ((stringp v)    (write-u8 s +tag-string+) (write-bstring s v))
    ((keywordp v)   (write-u8 s +tag-keyword+) (write-bstring s (symbol-name v)))
    ((listp v)      (write-u8 s +tag-list+) (write-u32 s (length v))
                    (dolist (e v) (stream-write-value s e)))
    (t              (write-u8 s +tag-string+) (write-bstring s (princ-to-string v)))))

(defun stream-read-value (s)
  (ecase (read-u8 s)
    (#.+tag-nil+     nil)
    (#.+tag-t+       t)
    (#.+tag-int+     (read-i64 s))
    (#.+tag-string+  (read-bstring s))
    (#.+tag-keyword+ (intern (read-bstring s) :keyword))
    (#.+tag-list+    (let ((n (read-u32 s)))
                       (loop repeat n collect (stream-read-value s))))))

;;; --- streaming views via externalize/internalize ---------------------------

(defun stream-write-view (s view)
  "Serialise VIEW (via EXTERNALIZE) onto binary stream S."
  (stream-write-value s (externalize view)))

(defun stream-read-view (s)
  "Read a view from binary stream S (via INTERNALIZE)."
  (internalize (stream-read-value s)))

;;; --- resource file ---------------------------------------------------------

(defparameter +resource-magic+
  (sb-ext:string-to-octets "TVRC" :external-format :latin-1))
(defparameter +resource-version+ 1)

(defclass tresource-file ()
  ((table :initform (make-hash-table :test 'equal) :accessor resource-table)))

(defun make-resource-file () (make-instance 'tresource-file))

(defun resource-put (rf name value)
  "Store VALUE (any tagged value) under NAME."
  (setf (gethash name (resource-table rf)) value))
(defun resource-get (rf name) (gethash name (resource-table rf)))
(defun resource-names (rf)
  (loop for k being the hash-keys of (resource-table rf) collect k))

(defun resource-put-object (rf name view)
  "Store VIEW under NAME (externalised)."
  (resource-put rf name (externalize view)))
(defun resource-get-object (rf name)
  "Reconstruct the view stored under NAME, or NIL."
  (let ((v (resource-get rf name))) (and v (internalize v))))

(defun save-resource-file (path rf)
  "Write resource file RF to PATH in binary."
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede :if-does-not-exist :create)
    (write-sequence +resource-magic+ s)
    (write-u8 s +resource-version+)
    (write-u32 s (hash-table-count (resource-table rf)))
    (maphash (lambda (k v) (write-bstring s k) (stream-write-value s v))
             (resource-table rf)))
  path)

(defun load-resource-file (path)
  "Read a resource file from PATH.  Return a TRESOURCE-FILE, or NIL."
  (with-open-file (s path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (when s
      (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
        (read-sequence magic s)
        (unless (equalp magic +resource-magic+) (return-from load-resource-file nil)))
      (read-u8 s)                          ; version (ignored for now)
      (let ((rf (make-resource-file)) (n (read-u32 s)))
        (dotimes (i n)
          (let ((k (read-bstring s))) (resource-put rf k (stream-read-value s))))
        rf))))
