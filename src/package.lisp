;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(defpackage #:cl-nailgun
  (:use #:cl #:iolib #:trivial-gray-streams #:bordeaux-threads #:alexandria #:flexi-streams)
  (:import-from #:arnesi #:with-collector)
  (:import-from #:nibbles #:ub32ref/be #:write-ub32/be #:make-octet-vector)
  (:shadowing-import-from #:babel #:octets-to-string)
  (:export ;;; main entry points
           #:run-server
           #:exit

           ;;; settings
           #:*default-buffer-size*
           #:*max-buffer-size*))
