;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-nailgun-script; -*-

(in-package #:cl-nailgun-script)

(defsynopsis (:postfix "FILES...")
  (text :contents "A very short program.")
  (group (:header "Immediate exit options:")
    (flag :short-name "h" :long-name "help"
          :description "Print this help and exit.")
    (flag :short-name "v" :long-name "version"
          :description "Print version number and exit.")))

(defun main (command arguments directory environment output error input)
  (let ((*standard-output* output)
        (*error-output* error)
        (*standard-input* input)
        (*trace-output* output))
    (make-context :cmdline (cons command arguments))
    (when (getopt :short-name "h")
      (help)
      (exit))
    (do-cmdline-options (option name value source)
      (print (list option name value source)))
    (terpri)
    (loop
      (format T "~A~%" (handler-case (eval (or (read *standard-input* NIL) (return)))
                         (error (error) error))))))

(defun start ()
  (run-server #'main))
