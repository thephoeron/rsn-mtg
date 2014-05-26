;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RSN-MTG; Base: 10 -*- file: rsn-mtg.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :rsn-mtg)

(defun initial-setup (&key (db *default-database-connection*) (verbose nil))
  "Perform the initial set-up operations to configure your web app's environment for the RSN-MTG library."
  (format t "~%;; SETTING UP YOUR ENVIRONMENT FOR RSN-MTG LIBRARY")
  (postmodern:with-connection db
    (postmodern:create-package-tables 'rsn-mtg)
    (sync-db-from-gatherer :db db :verbose verbose))
  (format t "~%;; Done.  Please restart your REDSHIFTNET-powered Web App now."))

;; EOF
