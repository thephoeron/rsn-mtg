;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RSN-MTG; Base: 10 -*- file: packages.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage #:rsn-mtg
    (:nicknames #:mtg)
    (:use :cl 
          :hunchentoot
          :cl-who
          :cl-css
          :postmodern)
    (:export #:*rsn-mtg-version*
             #:sync-db-from-gatherer))

;; see asdf system definition
(defvar rsn-mtg:*rsn-mtg-version*
  #.rsn-mtg-asd::*rsn-mtg-version*)

;; EOF
