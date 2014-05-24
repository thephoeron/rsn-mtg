;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RSN-MTG; Base: 10 -*-
;;;; file: scraper.lisp

;;;; Copyright (c) 2013--2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :rsn-mtg)

;; Utility functions to support Gatherer DB web scraper

(defun strip-string (str)
  "Strip linebreaks and trim whitespace from a string str."
  (string-trim '(#\Space #\Tab #\Newline)
    (remove (code-char #x0a)
      (remove (code-char #x0d)
        str))))

;; Predicate Tests for STP objects

(defun class-row-p (obj)
  "Test to verify object *obj* is an STP element div with class attribute value 'row'."
  (and (typep obj 'stp:element)
       (equal (stp:local-name obj) "div")
       (equal (stp:attribute-value obj "class") "row")))

(defun class-label-p (obj)
  "Test to verify object *obj* is an STP element div with class attribute value 'label'."
  (and (typep obj 'stp:element)
       (equal (stp:local-name obj) "div")
       (equal (stp:attribute-value obj "class") "label")))

(defun class-value-p (obj)
  "Test to verify object *obj* is an STP element div with class attribute value 'value'."
  (and (typep obj 'stp:element)
       (equal (stp:local-name obj) "div")
       (equal (stp:attribute-value obj "class") "value")))

(defun img-tag-p (obj)
  "Test to verify object *obj* is an STP element img."
  (and (typep obj 'stp:element)
       (equal (stp:local-name obj) "img")))

;; The Gatherer DB web-scraper

(defun card-exists-p (m-id &key (db *default-database-connection*))
  "Queries the local database to test if the card for Multiverse ID *m-id* is already in the database."
  nil)

(defun card-matches-p (m-id &key (db *default-database-connection*))
  "Compares the card DAO in the local database against Gatherer to see if all the fields match."
  nil)

(defun valid-m-id-p (m-id)
  "Queries the Gatherer database to test if Multiverse ID *m-id* returns a card or an error."
  t)

(defun scrape-gatherer-and-insert-mtg-card-into-db (m-id &key (db *default-database-connection*))
  "Query the Gatherer database for the Multiverse ID *m-id* card, parse all data, and if the record does not already exist, store in the local database."
  (let* ((m-id-str (format nil "~D" m-id))
         (query (list (cons "multiverseid" m-id-str)))
         (str (drakma:http-request "http://gatherer.wizards.com/Pages/Card/Details.aspx"
                                   :parameters query))
         (document (chtml:parse str (cxml-stp:make-builder)))
         (name nil)
         (mana-cost nil)
         (color nil)
         (spell-type nil)
         (collectors-num nil)
         (converted-mana-cost nil)
         (card-text nil)
         (flavor-text nil)
         (power nil)
         (toughness nil)
         (set-name nil)
         (rarity nil)
         (artist nil)
         (image nil)
         (full-image-path nil)
         (the-set-id nil)
         (the-card nil))
    (format t "~%;; Inserting Multiverse ID: ~D, " m-id)
    (stp:do-recursively (div document)
      (when (class-row-p div)
        (cond ((search "nameRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (setf name (strip-string (stp:string-value tag)))
                   (format t "Card Name: ~A, " name))))
              ((search "manaRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (stp:do-children (img tag)
                     (when (img-tag-p img)
                       (setf mana-cost (format nil "~@{~A~^ ~}" (if mana-cost mana-cost "") (stp:attribute-value img "alt")))))))
                (let ((mana-list (split-sequence #\Space mana-cost))
                      (color-list '("White" "Blue" "Black" "Red" "Green"))
                      (color-count 0))
                  (loop for color in color-list
                        do (when (search color mana-cost)
                             (incf color-count)))
                  (cond ((= color-count 0)
                         (setf color "Colorless"))
                        ((= color-count 1)
                         (setf color (cond ((search "White" mana-cost) "White")
                                           ((search "Blue" mana-cost) "Blue")
                                           ((search "Black" mana-cost) "Black")
                                           ((search "Red" mana-cost) "Red")
                                           ((search "Green" mana-cost) "Green")
                                           (t nil))))
                        ((>= color-count 2)
                         (setf color "Gold"))
                        (t nil))
                  (format t "Mana: ~A, Color: ~A, " (string-trim '(#\Space #\Tab) mana-cost)
                                                       color)))
              ((search "cmcRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (setf converted-mana-cost (strip-string (stp:string-value tag)))
                   (format t "CMC: ~A, " converted-mana-cost))))
              ((search "typeRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (setf spell-type (strip-string (stp:string-value tag)))
                   (format t "Type: ~A, " spell-type))))
              ((search "setRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (setf set-name (strip-string (stp:string-value tag)))
                   ; (setf the-set-id (get-expansion-id-by-name set-name))
                   (format t "Set: ~A. " set-name))))
              (t nil))))
    (setf image (format nil "~D-~A.jpg" m-id (string-downcase (substitute #\- #\Space name))))
    (setf full-image-path (concatenate 'string (namestring *default-img-dir*) image))
    ;; Uncomment following form to enable downloading of MTG Card images
    ; (with-open-file (file full-image-path
    ;                       :direction :output
    ;                       :if-does-not-exist :create
    ;                       :if-exists :supersede
    ;                       :element-type '(unsigned-byte 8))
    ;   (let* ((query (list (cons "multiverseid" m-id-str)
    ;                       (cons "type" "card")))
    ;          (input (drakma:http-request "http://gatherer.wizards.com/Handlers/Image.ashx"
    ;                                     :parameters query
    ;                                     :want-stream t)))
    ;     (awhile (read-byte input nil nil)
    ;       (write-byte it file))
    ;     (close input))
    ;     (format t "Card image ~A successfuly downloaded." image))
    (format t "~C[32;1m~C~C[0m" #\Escape (code-char #x2714) #\Escape)))

(defun scrape-gatherer-and-update-mtg-card-in-db (m-id &key (db *default-database-connection*))
  "Update an existing local database record for Multiverse ID *m-id* card from Gatherer. Triggers a condition if DAO for *m-id* does not exist."
  (format t "~%;; Updating Multiverse ID: ~D DAO record... " m-id)
  (format t "~C[32;1m~C~C[0m" #\Escape (code-char #x2714) #\Escape))

(defun sync-db-from-gatherer (&key (db *default-database-connection*))
  "Calls the web-scraper to search the official Gatherer DB, and builds DAOs for each object not in the local database."
  (format t "~%;; Syncing local database from Gatherer...")
  (loop for i below 12 ; 0000000
        with m-id = (+ i 1)
        do (cond ((not m-id) nil)
                 ((not (valid-m-id-p m-id))
                  (format t "~%;; Multiverse ID: ~D does not specify a valid MTG Card. Skipping..." m-id))
                 ((and (card-exists-p m-id)
                       (card-matches-p m-id))
                  nil)
                 ((card-exists-p m-id)
                  (scrape-gatherer-and-update-mtg-card-in-db m-id))
                 ((valid-m-id-p m-id)
                  (scrape-gatherer-and-insert-mtg-card-into-db m-id))
                 (t (loop-finish)))
           (incf m-id)))

;; EOF
