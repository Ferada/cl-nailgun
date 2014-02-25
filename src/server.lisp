;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-nailgun; -*-

(in-package #:cl-nailgun)

(defvar *default-buffer-size* 128)

(defvar *clients*)
(defvar *event-base*)

(defun run-server (handler &key (port 2113))
  (let (*clients* *event-base*)
    (unwind-protect
         (progn
           (setf *clients* (make-hash-table :test 'eq)
                 *event-base* (make-instance 'event-base))
           (with-open-socket
               (server :connect :passive
                       :address-family :internet
                       :type :stream
                       :ipv6 NIL)
             (bind-address server +ipv4-unspecified+ :port port :reuse-addr T)
             (listen-on server :backlog 5)
             (set-io-handler
              *event-base*
              (socket-os-fd server)
              :read
              (lambda (fd event exception)
                (declare (ignore fd event exception))
                (let ((socket (accept-connection server :wait NIL :element-type 'unsigned-byte)))
                  (when socket
                    (setf (gethash socket *clients*)
                          (make-thread
                           (lambda ()
                             (unwind-protect
                                  (handle-client-prolog
                                   socket
                                   (make-array *default-buffer-size* :element-type '(unsigned-byte 8))
                                   handler)
                               (close socket)
                               (remhash socket *clients*)))
                           :name (format NIL "nailgun-client/~A" (remote-name socket))
                           :initial-bindings `((*standard-output* . ,*standard-output*)
                                               (*clients* . ,*clients*))))))))
             (event-dispatch *event-base*)))
      (maphash
       (lambda (socket thread)
         (close socket :abort T)
         (destroy-thread thread))
       *clients*)
      (when *event-base*
        (close *event-base*)))))

(defun handle-client-prolog (socket buffer handler)
  (let (environment directory command)
    (with-collector (arguments)
      (loop
        (let ((read-index (read-sequence buffer socket :end 5)))
          (unless (eql read-index 5)
            (error 'end-of-file :stream socket)))
        (let* ((length (parse-length buffer))
               (type (parse-type buffer)))
          (when (> length (length buffer))
            (setf buffer (make-array length :element-type '(unsigned-byte 8))))
          (let ((read-index (read-sequence buffer socket :end length)))
            (unless (eql read-index length)
              (error 'end-of-file :stream socket)))
          (let ((string (octets-to-string buffer :end length)))
            (ecase type
              (:environment
               (push (let ((position (position #\= string)))
                       (cons (subseq string 0 position)
                             (subseq string (1+ position))))
                     environment))
              (:argument
               (arguments string))
              (:directory
               (setf directory string))
              (:command
               (setf command string)
               (return))))))
      (funcall handler
               command (arguments) directory environment
               (make-flexi-stream
                (make-instance 'nailgun-binary-output-stream
                               :socket socket
                               :fd :stdout))
               (make-flexi-stream
                (make-instance 'nailgun-binary-output-stream
                               :socket socket
                               :fd :stderr))
               (make-flexi-stream
                (make-instance 'nailgun-binary-input-stream
                               :socket socket
                               :buffer buffer))))))

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
    :initform 0)))

(defclass nailgun-binary-output-stream (nailgun-binary-stream
                                        fundamental-binary-output-stream)
  ((fd
    :initarg :fd)))

(defmethod stream-element-type ((stream nailgun-binary-input-stream))
  '(unsigned-byte 8))

(defmethod stream-read-byte ((stream nailgun-binary-input-stream))
  (with-slots (socket state buffer start end) stream
    (when (eq state :eof)
      (return-from stream-read-byte :eof))
    (when (> end start)
      (return-from stream-read-byte
        (aref buffer (prog1 start
                       (incf start)
                       (when (eql start end)
                         (setf start (setf end 0)))))))
    (write-length 0 socket)
    (write-type :start-input socket)
    (finish-output socket)
    (setf state :start-input)
    (loop
      (let ((read-index (read-sequence buffer socket :end 5)))
        (unless (eql read-index 5)
          (setf state :eof)
          (error 'end-of-file :stream socket)))
      (let* ((length (parse-length buffer))
             (type (parse-type buffer)))
        (setf start 0)
        (setf end length)
        (when (> end (length buffer))
          (setf buffer (make-array end :element-type '(unsigned-byte 8))))
        (let ((read-index (read-sequence buffer socket :start start :end end)))
          (unless (eql read-index end)
            (setf state :eof)
            (error 'end-of-file :stream socket)))
        (ecase type
          (:stdin
           (return-from stream-read-byte (stream-read-byte stream)))
          (:heartbeat)
          (:eof
           (setf state 'end-of-file)
           (return-from stream-read-byte :eof)))))))

(defmethod stream-element-type ((stream nailgun-binary-output-stream))
  '(unsigned-byte 8))

(defmethod stream-write-byte ((stream nailgun-binary-output-stream) integer)
  (let ((socket (slot-value stream 'socket)))
    (write-length 1 socket)
    (write-type (slot-value stream 'fd) socket)
    (write-byte integer socket)))

(defmethod stream-write-sequence ((stream nailgun-binary-output-stream) sequence start end &key)
  (let ((socket (slot-value stream 'socket)))
    (write-length (length sequence) socket)
    (write-type (slot-value stream 'fd) socket)
    (write-sequence sequence socket)))

(defun test-handler (command arguments directory environment output error input)
  (logv:format-log "called as ~A ~{~A~^ ~} in ~A" command arguments directory)
  (format output "Hello, World!~%")
  (format error "Errors go here!~%")
  (loop
    (let ((line (read-line input NIL)))
      (unless line
        (return))
      (format output "~S~%" line))))

(defun parse-length (header)
  (+ (ash (aref header 0) 24)
     (ash (aref header 1) 16)
     (ash (aref header 2) 8)
     (aref header 3)))

(defun unparse-length (length)
  (values (ash length -24)
          (logand (ash length -16) #xff)
          (logand (ash length -8) #xff)
          (logand length #xff)))

(defun write-length (length stream)
  (multiple-value-bind (a b c d)
      (unparse-length length)
    (write-byte a stream)
    (write-byte b stream)
    (write-byte c stream)
    (write-byte d stream)))

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
