;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-nailgun
  :description "Nailgun server"
  :long-description "Remotely hosted command line programs for Common Lisp."
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :license "Simplified BSD License"
  :version "0.0.2"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (#:bordeaux-threads #:iolib #:trivial-gray-streams #:babel #:alexandria #:flexi-streams #:nibbles #:arnesi)
  :in-order-to ((asdf:test-op (asdf:load-op #:cl-nailgun-tests)))
  :perform (asdf:test-op :after (op c)
             (funcall (find-symbol (symbol-name '#:run!) '#:fiveam)
                      (find-symbol (symbol-name '#:cl-nailgun) '#:cl-nailgun-tests)))
  :serial T
  :components ((:static-file "README.md")
               (:module "src"
                :components
                ((:file "package")
                 (:file "server")))))
