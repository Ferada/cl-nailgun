;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-nailgun-script; -*-

(in-package #:cl-nailgun-script)

(defsynopsis (:postfix "FILES...")
  (text :contents "A very short program.")
  (group (:header "Immediate exit options:")
    (flag :short-name "h" :long-name "help"
          :description "Print this help and exit.")
    (flag :short-name "v" :long-name "version"
          :description "Print version number and exit.")))

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
    (make-context :cmdline (cons command arguments))
    (when (getopt :short-name "h")
      (help)
      (exit))
    (do-cmdline-options (option name value source)
      (print (list option name value source)))
    (terpri)
    (loop
      (format T "~A~%" (handler-case (eval (or (read T NIL) (return)))
                         (error (error) error))))))

(defun start (&optional (port 2323))
  (run-server #'main :port port))
