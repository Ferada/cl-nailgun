;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-nailgun; -*-

(in-package #:cl-nailgun)

(defvar *default-buffer-size* 128)
(defvar *max-buffer-size* 32)

(defvar *clients*)
(defvar *event-base*)

(declaim (inline make-buffer))

(defun make-buffer (size)
  (make-octet-vector size))

(defun make-server-accept-handler (server handler environmentp)
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (let ((socket (accept-connection server :wait NIL :element-type 'octet)))
      (when socket
        (setf (gethash socket *clients*)
              (make-thread
               (lambda ()
                 (unwind-protect
                      (handle-client-prolog
                       socket
                       (make-buffer *default-buffer-size*)
                       handler
                       environmentp)
                   (close socket)
                   (remhash socket *clients*)))
               :name (format NIL "nailgun-client/~A" (remote-name socket))
               :initial-bindings `((*standard-output* . ,*standard-output*)
                                   (*error-output* . ,*error-output*)
                                   (*standard-input* . ,*standard-input*)
                                   (*trace-output* . ,*trace-output*)
                                   (*terminal-io* . ,*terminal-io*)
                                   (*debug-io* . ,*debug-io*)
                                   (*clients* . ,*clients*))))))))

(defun run-server (handler &key (port 2113) (environmentp T))
  (let (*clients* *event-base*)
    (unwind-protect
         (progn
           ;; TODO: synchronized?
           (setf *clients* (make-hash-table :test 'eq)
                 *event-base* (make-instance 'event-base))
           (with-open-socket
               (server :connect :passive
                       :address-family :internet
                       :type :stream
                       :ipv6 T)
             (bind-address server +ipv6-unspecified+ :port port :reuse-addr T)
             (listen-on server :backlog 5)
             (set-io-handler
              *event-base*
              (socket-os-fd server)
              :read
              (make-server-accept-handler server handler environmentp))
             (event-dispatch *event-base*)))
      (maphash
       (lambda (socket thread)
         (close socket :abort T)
         (destroy-thread thread))
       *clients*)
      (when *event-base*
        (close *event-base*))))
  (values))

(declaim (inline parse-length write-length))

(defun parse-length (header)
  (ub32ref/be header 0))

(defun write-length (length stream)
  (write-ub32/be length stream))

(defconstant +header-length+ 5)

(defun getenv (name list)
  (dolist (string list)
    (let ((position (position #\= string)))
      (when (and (eql (length name) position)
                 (string= name string :end2 position))
        (return (subseq string (1+ position)))))))

(defun collect-client-prolog (socket buffer environmentp)
  (let (environment directory command)
    (with-collector (arguments)
      (loop
        (let ((read-index (read-sequence buffer socket :end +header-length+)))
          (unless (eql read-index +header-length+)
            (error 'end-of-file :stream socket)))
        (let* ((length (parse-length buffer))
               (type (parse-type buffer)))
          (when (> length (length buffer))
            (setf buffer (make-buffer length)))
          (let ((read-index (read-sequence buffer socket :end length)))
            (unless (eql read-index length)
              (error 'end-of-file :stream socket)))
          (let (string)
            (unless (and (eq type :environment)
                         (not environmentp))
              (setf string (octets-to-string buffer :end length)))
            (ecase type
              (:environment
               (when environmentp
                 (push string environment)))
              (:argument
               (arguments string))
              (:directory
               (setf directory string))
              (:command
               (setf command string)
               (return))))))
      (values environment directory command (arguments)))))

(let ((exit (gensym)))
  (defun exit (&optional (code 0))
    (throw exit code))

  (defun handle-client-prolog (socket buffer handler environmentp)
    (multiple-value-bind (environment directory command arguments)
        (collect-client-prolog socket buffer environmentp)
      (flet ((aux (outputp &rest args)
               (make-flexi-stream
                (apply #'make-instance
                       (if outputp
                           'nailgun-binary-output-stream
                           'nailgun-binary-input-stream)
                       :socket socket
                       args))))
        (let ((exit-code 1)
              stdout stderr stdin)
          (unwind-protect
               (setf exit-code
                     (catch exit
                       (prog1 0
                         (funcall
                          handler
                          command arguments directory environment
                          (lambda (name)
                            (ecase name
                              (:stdout
                               (or stdout (setf stdout (aux T :fd :stdout))))
                              (:stderr
                               (or stderr (setf stdout (aux T :fd :stderr))))
                              (:stdin
                               (or stdin (setf stdin (aux NIL :buffer buffer))))))))))
            (when stdout
              (close stdout))
            (when stderr
              (close stderr))
            (when stdin
              (close stdin))
            (when (open-stream-p socket)
              (let ((sequence (string-to-octets (format NIL "~D" exit-code))))
                (write-length (length sequence) socket)
                (write-type :exit socket)
                (write-sequence sequence socket)
                (finish-output socket)))))))))

(defclass nailgun-binary-stream ()
  ((socket
    :initarg :socket)))

(defmethod stream-finish-output ((stream nailgun-binary-stream))
  (finish-output (slot-value stream 'socket)))

(defmethod stream-force-output ((stream nailgun-binary-stream))
  (force-output (slot-value stream 'socket)))

(defclass nailgun-binary-input-stream (nailgun-binary-stream
                                       fundamental-binary-input-stream)
  ((state
    :initform NIL)
   (buffer
    :initarg :buffer)
   (start
    :initform 0)
   (end
    :initform 0)
   (still-reading
    :initform 0)))

(defclass nailgun-binary-output-stream (nailgun-binary-stream
                                        fundamental-binary-output-stream)
  ((fd
    :initarg :fd)))

(defmethod stream-element-type ((stream nailgun-binary-stream))
  'octet)

(defmethod stream-read-byte ((stream nailgun-binary-input-stream))
  (with-slots ((buffer-slot buffer) (still-reading-slot still-reading)
               (start-slot start) (end-slot end)
               socket state)
      stream
    (let* ((socket socket)
           (buffer buffer-slot)
           (buffer-length (length buffer))
           (still-reading still-reading-slot)
           (start start-slot)
           (end end-slot))
      (block NIL
        (tagbody
           (when (eq state :eof)
             (go :eof))
         :stdin
           (when (> end start)
             (return
               (aref buffer (prog1 start
                              (incf start)
                              (when (eql start end)
                                (setf start (setf end-slot (setf end 0))))
                              (setf start-slot start)))))
           (unless (eql still-reading 0)
             (let* ((read (min still-reading buffer-length))
                    (read-index (read-sequence buffer socket :end read)))
               (unless (eql read-index read)
                 (go :eof))
               (setf still-reading (decf still-reading-slot read))
               (setf start (setf start-slot 0))
               (setf end (setf end-slot read)))
             (go :stdin))
           (write-length 0 socket)
           (write-type :start-input socket)
           (finish-output socket)
           (setf state :start-input)
         :wait
           (let ((read-index (read-sequence buffer socket :end +header-length+)))
             (unless (eql read-index +header-length+)
               (go :eof)))
           (setf start-slot (setf start 0))
           (setf end-slot (setf end (parse-length buffer)))
           (let ((type (parse-type buffer)))
             (when (> end buffer-length)
               (let ((min (min end *max-buffer-size*)))
                 (unless (eql min buffer-length)
                   (setf buffer-slot (setf buffer (make-buffer (setf buffer-length min)))))))
             (when (> end buffer-length)
               (setf still-reading (setf still-reading-slot (- end buffer-length)))
               (setf end (setf end-slot buffer-length)))
             (let ((read-index (read-sequence buffer socket :end end)))
               (unless (eql read-index end)
                 (go :eof)))
             (ecase type
               (:stdin
                (go :stdin))
               ;; TODO: do what if a timeout occurs?
               (:heartbeat
                (go :wait))
               (:eof
                (go :eof))))
         :eof
           (return (setf state :eof)))))))

(defmethod stream-read-sequence ((stream nailgun-binary-input-stream) sequence seq-start seq-end &key)
  (when (>= seq-start seq-end)
    (return-from stream-read-sequence seq-start))
  (with-slots ((buffer-slot buffer) (still-reading-slot still-reading)
               (start-slot start) (end-slot end)
               socket state)
      stream
    (let* ((socket socket)
           (buffer buffer-slot)
           (buffer-length (length buffer))
           (still-reading still-reading-slot)
           (start start-slot)
           (end end-slot))
      (block NIL
        (tagbody
           (when (eq state :eof)
             (go :eof))
         :stdin
           (when (> end start)
             (replace sequence buffer
                      :start1 seq-start :end1 seq-end
                      :start2 start :end2 end)
             (let ((copied (min (- seq-end seq-start)
                                (- end start))))
               (incf seq-start copied)
               (setf start-slot (incf start copied))
               (when (eql seq-start seq-end)
                 (return seq-start))))
           (unless (eql still-reading 0)
             (let* ((read (min still-reading buffer-length))
                    (read-index (read-sequence buffer socket :end read)))
               (unless (eql read-index read)
                 (go :eof))
               (setf still-reading (decf still-reading-slot read))
               (setf start (setf start-slot 0))
               (setf end (setf end-slot read)))
             (go :stdin))
           (write-length 0 socket)
           (write-type :start-input socket)
           (finish-output socket)
           (setf state :start-input)
         :wait
           (let ((read-index (read-sequence buffer socket :end +header-length+)))
             (unless (eql read-index +header-length+)
               (go :eof)))
           (setf start-slot (setf start 0))
           (setf end-slot (setf end (parse-length buffer)))
           (let ((type (parse-type buffer)))
             (when (> end buffer-length)
               (let ((min (min end *max-buffer-size*)))
                 (unless (eql min buffer-length)
                   (setf buffer-slot (setf buffer (make-buffer (setf buffer-length min)))))))
             (when (> end buffer-length)
               (setf still-reading (setf still-reading-slot (- end buffer-length)))
               (setf end (setf end-slot buffer-length)))
             (let ((read-index (read-sequence buffer socket :end end)))
               (unless (eql read-index end)
                 (go :eof)))
             (ecase type
               (:stdin
                (go :stdin))
               ;; TODO: do what if a timeout occurs?
               (:heartbeat
                (go :wait))
               (:eof
                (go :eof))))
         :eof
           (setf state :eof)
           (return seq-start))))))

(defmethod stream-write-byte ((stream nailgun-binary-output-stream) integer)
  (with-slots (socket fd) stream
    (write-length 1 socket)
    (write-type fd socket)
    (write-byte integer socket)))

(defmethod stream-write-sequence ((stream nailgun-binary-output-stream) sequence start end &key)
  (when (>= start end)
    (return-from stream-write-sequence sequence))
  (with-slots (socket fd) stream
    (write-length (- end start) socket)
    (write-type fd socket)
    (write-sequence sequence socket :start start :end end)))

(macrolet
      ((aux (&rest forms)
         `(progn
            (defun parse-type (header)
              (ecase (aref header 4)
                ,@(mapcar (lambda (form)
                            `(,(char-code (car form))
                              ,(cadr form)))
                   forms)))
            (defun unparse-type (type)
              (ecase type
                ,@(mapcar (lambda (form)
                            `(,(cadr form)
                              ,(char-code (car form))))
                   forms)))
            (defun write-type (type stream)
              (write-byte (unparse-type type) stream)))))
    (aux
     (#\0 :stdin)
     (#\A :argument)
     (#\E :environment)
     (#\D :directory)
     (#\C :command)
     (#\1 :stdout)
     (#\2 :stderr)
     (#\S :start-input)
     (#\. :eof)
     (#\X :exit)
     ;; extended, i.e. the current client does this
     (#\L :long-argument)
     (#\H :heartbeat)))
