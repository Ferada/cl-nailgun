;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(defpackage #:cl-nailgun-script
  (:use #:cl #:cl-nailgun #:unix-options)
  (:shadowing-import-from #:cl-nailgun #:exit)
  (:export ;;; main entry points
           #:start
           #:main))
