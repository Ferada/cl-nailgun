;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(defpackage #:cl-nailgun-script
  (:use #:cl #:cl-nailgun)
  (:export ;;; main entry points
           #:start
           #:main))
