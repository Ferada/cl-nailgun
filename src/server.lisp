;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-nailgun; -*-

(in-package #:cl-nailgun)

(defvar *default-buffer-size* 128)

(defvar *clients*)
(defvar *event-base*)

(declaim (inline make-buffer))

(defun make-buffer (size)
  (make-octet-vector size))

(defun make-server-accept-handler (server handler)
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
                       handler)
                   (close socket)
                   (remhash socket *clients*)))
               :name (format NIL "nailgun-client/~A" (remote-name socket))
               :initial-bindings `((*standard-output* . ,*standard-output*)
                                   (*clients* . ,*clients*))))))))

(defun run-server (handler &key (port 2113))
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
              (make-server-accept-handler server handler))
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

(defun collect-client-prolog (socket buffer)
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
      (values environment directory command (arguments)))))

(defun handle-client-prolog (socket buffer handler)
  (multiple-value-bind (environment directory command arguments)
      (collect-client-prolog socket buffer)
    (funcall handler
             command arguments directory environment
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
                             :buffer buffer)))))

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

(defmethod stream-element-type ((stream nailgun-binary-stream))
  'octet)

(defmethod stream-read-byte ((stream nailgun-binary-input-stream))
  (with-slots ((buffer-slot buffer) (start-slot start) (end-slot end)
               socket state)
      stream
    (let ((socket socket)
          (buffer buffer-slot)
          (start start-slot)
          (end end-slot))
      (block NIL
        (tagbody
           (when (eq state :eof)
             (return :eof))
         :stdin
           (when (> end start)
             (return
               (aref buffer (prog1 start
                              (incf start)
                              (when (eql start end)
                                (setf start (setf end-slot (setf end 0))))
                              (setf start-slot start)))))
           (write-length 0 socket)
           (write-type :start-input socket)
           (finish-output socket)
           (setf state :start-input)
         :wait
           (let ((read-index (read-sequence buffer socket :end +header-length+)))
             (unless (eql read-index +header-length+)
               (setf state :eof)
               (error 'end-of-file :stream socket)))
           (setf start-slot (setf start 0))
           (setf end-slot (setf end (parse-length buffer)))
           (when (> end (length buffer))
             (setf buffer-slot (setf buffer (make-buffer end))))
           (let ((read-index (read-sequence buffer socket :end end)))
             (unless (eql read-index end)
               (setf state :eof)
               (error 'end-of-file :stream socket)))
           (ecase (parse-type buffer)
             (:stdin
              (go :stdin))
             ;; TODO: do what if a timeout occurs?
             (:heartbeat
              (go :wait))
             (:eof
              (return (setf state :eof)))))))))

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

(defun test-handler (command arguments directory environment output error input)
  (logv:format-log "called as ~A ~{~A~^ ~} in ~A" command arguments directory)
  (format output "Hello, World!~%")
  (format error "Errors go here!~%")
  (loop
    (let ((line (read-line input NIL)))
      (unless line
        (return))
      (format output "~S~%" line))))

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
