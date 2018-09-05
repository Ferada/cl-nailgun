;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-nailgun-script; -*-

(in-package #:cl-nailgun-script)

(defun main (command arguments directory environment streams)
  (let* ((*standard-output* (funcall streams :stdout))
         (*error-output* (funcall streams :stderr))
         (*standard-input* (funcall streams :stdin))
         (*terminal-io* (make-two-way-stream
                         *standard-input*
                         *standard-output*))
         (directory (pathname directory))
         (*default-pathname-defaults*
           (make-pathname :directory (append (pathname-directory directory)
                                             (list (file-namestring directory)))
                          :name NIL :type NIL :version NIL
                          :defaults directory)))
    (with-cli-options (arguments "A very short program.~%~%Usage:~%~@{~A~%~}~%")
                      (version &parameters)
      (cond
        (version
         (format T "0.0.1~%"))
        (T
         (format T "~A ~A ~A~%" command free environment)
         (loop
           (format T "~A~%" (handler-case (eval (or (read T NIL) (return)))
                              (error (error) error)))))))))

(defun start (&optional (port 2323))
  (run-server #'main :port port))
