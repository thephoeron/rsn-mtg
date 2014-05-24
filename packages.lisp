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
          :postmodern
          :cl-fad
          :split-sequence
          :arnesi)
    (:export #:*rsn-mtg-version*
             #:*default-dir*
             #:*default-img-dir*
             #:sync-db-from-gatherer))

;; see asdf system definition
(defvar rsn-mtg::*rsn-mtg-version*
  #.rsn-mtg-asd::*rsn-mtg-version*)

(defvar rsn-mtg::*default-dir*
  (pathname (directory-namestring #.(or *compile-file-truename*
                                        *load-truename*)))
    "The directory path of the current file.")

(defvar rsn-mtg::*default-img-dir*
  (merge-pathnames "static/images/rsn-mtg/" rsn-mtg:*default-dir*))

(ensure-directories-exist rsn-mtg::*default-img-dir*)

;; EOF
