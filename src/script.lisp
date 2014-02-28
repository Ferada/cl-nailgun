;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-nailgun-script; -*-

(in-package #:cl-nailgun-script)

(defun main (command arguments directory environment output error input)
  (let ((*standard-output* output)
        (*error-output* error)
        (*standard-input* input)
        (*trace-output* output))
    (loop
      (format T "~A~%" (handler-case (eval (or (read *standard-input* NIL) (return)))
                         (error (error) error))))))

(defun start ()
  (run-server #'main))
