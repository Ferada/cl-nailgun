;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-nailgun-script
  :description "Nailgun scripting host"
  :long-description "Persistent Common Lisp scripting daemon via nailgun."
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :license "Simplified BSD License"
  :version "0.0.1"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (#:cl-nailgun #:com.dvlsoft.clon)
  :serial T
  :components ((:static-file "README.md")
               (:module "src"
                :components
                ((:file "package-script")
                 (:file "script")))))
