;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RSN-MTG; Base: 10 -*- file: rsn-mtg.asd

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage rsn-mtg-asd
    (:use :cl :asdf)
    (:export #:*rsn-mtg-version*))

(in-package :rsn-mtg-asd)

(defparameter *rsn-mtg-version* "1.0.0")

(defsystem rsn-mtg
    :version #.*rsn-mtg-version*
    :author "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
    :license "MIT"
    :description "RSN-MTG: A front-facing and administrative module for your REDSHIFTNET-powered web application, to track, catalogue, share, and view stats about your MTG card collection."
    :serial t
    :depends-on (:hunchentoot
                 :cl-who
                 :cl-css
                 :parenscript
                 :postmodern
                 :redshiftnet
                 :drakma
                 :closure-html
                 :cxml
                 :cxml-stp
                 :cl-fad
                 :cl-ppcre)
    :components ((:file "packages")
                 (:file "db")
                 (:file "scripts")
                 (:file "styles")
                 (:file "templates")
                 (:file "requests")
                 (:file "rsn-mtg")))

;; EOF