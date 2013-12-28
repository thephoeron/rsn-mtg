;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RSN-MTG; Base: 10 -*- file: rsn-mtg.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :rsn-mtg)

(defun sync-db-from-gatherer ()
  "Calls the web-scraper to search the official Gatherer DB, and builds DAOs for each object not in the local database.")

;; EOF
