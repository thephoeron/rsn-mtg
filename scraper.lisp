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

;; from Common Lisp Cookbook -- Strings: http://cl-cookbook.sourceforge.net/strings.html#manip
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

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

(defun class-cardtextbox-p (obj)
  "Test to verify object *obj* is an STP element div with class attribute value 'cardtextbox'."
  (and (typep obj 'stp:element)
       (equal (stp:local-name obj) "div")
       (equal (stp:attribute-value obj "class") "cardtextbox")))

(defun img-tag-p (obj)
  "Test to verify object *obj* is an STP element img."
  (and (typep obj 'stp:element)
       (equal (stp:local-name obj) "img")))

;; The Gatherer DB web-scraper
(defvar *gatherer-card-url* "http://gatherer.wizards.com/Pages/Card/Details.aspx")
(defvar *gatherer-img-url* "http://gatherer.wizards.com/Handlers/Image.ashx")

(defun parse-card (m-id document)
  "Use Closure HTML and CXML-STP packages to parse the Gatherer Database card detail pages and normalize the data. Returns multiple values, name, mana-cost, color, spell-type, collectors-num, converted-mana-cost, card-text, flavor-text, power, toughness, set-name, rarity, artist, and image."
  (let* ((m-id-str (format nil "~D" m-id))
         (name "none")
         (mana-cost "")
         (color "none")
         (spell-type "none")
         (collectors-num m-id)
         (converted-mana-cost 0)
         (card-text "none")
         (flavor-text "none")
         (power 0)
         (toughness 0)
         (set-name "none")
         (rarity "none")
         (artist "none")
         (image "none")
         (full-image-path nil)
         (the-card nil))
    (stp:do-recursively (div document)
      (when (class-row-p div)
        (cond ((search "nameRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (setf name (strip-string (stp:string-value tag)))
                   ; (format t "Card Name: ~A, " name)
                   )))
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
                  ; (format t "Mana: ~A, Color: ~A, " (string-trim '(#\Space #\Tab) mana-cost) color)
                  ))
              ((search "cmcRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (setf converted-mana-cost (parse-integer (strip-string (stp:string-value tag)) :junk-allowed t))
                   ; (format t "CMC: ~A, " converted-mana-cost)
                   )))
              ((search "typeRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (setf spell-type (strip-string (stp:string-value tag)))
                   ; (format t "Type: ~A, " spell-type)
                   )))
              ((search "textRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (let* ((text-pool ""))
                     (stp:do-children (par tag)
                       (when (class-cardtextbox-p par)
                         (stp:do-children (img par)
                           (if (img-tag-p img)
                               (setf text-pool (format nil "~@{~A~^ ~}" (if text-pool text-pool "") (format nil "[~A]" (stp:attribute-value img "alt"))))
                               (setf text-pool (format nil "~@{~A~^ ~}" (if text-pool text-pool "") (format nil "~A" (strip-string (stp:string-value img)))))))
                         (setf text-pool (format nil "~@{~A~^~}" (if text-pool text-pool "") "<br>"))))
                     (setf card-text (string-trim "<br> " text-pool)))
                   ; (if card-text (format t "Text: ~A, " card-text) (format t "Text: ~A, " "N"))
                   )))
              ((search "flavorRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (setf flavor-text (strip-string (stp:string-value tag)))
                   ; (if flavor-text (format t "Flavor: ~A, " flavor-text) (format t "Flavor: ~A, " "N"))
                   )))
              ((search "ptRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (let* ((pt-list (split-sequence #\/ (strip-string (stp:string-value tag))))
                          (pow (car pt-list))
                          (tuf (cadr pt-list)))
                     (setf power (if (search "*" pow)
                                     -1
                                     (handler-case
                                         (parse-integer (strip-string pow))
                                       (error () -406)))
                           toughness (if (search "*" tuf)
                                         -1
                                         (handler-case
                                             (parse-integer (strip-string tuf))
                                           (error () -406)))))
                   ; (format t "P/T: ~@{~A~^/~}, " power toughness)
                   )))
              ((search "setRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (setf set-name (strip-string (stp:string-value tag)))
                   ; (format t "Set: ~A, " set-name)
                   )))
              ((search "rarityRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (setf rarity (strip-string (stp:string-value tag)))
                   ; (format t "Rarity: ~A, " rarity)
                   )))
              ((search "numberRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (setf collectors-num (parse-integer (strip-string (stp:string-value tag)) :junk-allowed t))
                   ; (format t "CN: ~D, " collectors-num)
                   )))
              ((search "artistRow" (stp:attribute-value div "id") :from-end t)
               (stp:do-children (tag div)
                 (when (class-value-p tag)
                   (setf artist (strip-string (stp:string-value tag)))
                   ; (format t "Artist: ~A. " artist)
                   )))
              (t nil))))
    (setf image (format nil "~D-~A.jpg" m-id (string-downcase (substitute #\- #\Space name))))
    (return-from parse-card (list name mana-cost color spell-type collectors-num converted-mana-cost card-text flavor-text power toughness set-name rarity artist image))))

(defun download-and-save-card-image (m-id path)
  "Downloads and saves to disk the MTG card image for Multiverse ID *m-id* to Pathname *path*. Uses AWHILE macro from ARNESI package."
  (ensure-directories-exist path)
  (with-open-file (file path
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede
                        :element-type '(unsigned-byte 8))
    (let* ((m-id-str (format nil "~D" m-id))
           (pathn (pathname path))
           (image (format nil "~A.~A" (pathname-name pathn) (pathname-type pathn)))
           (query (list (cons "multiverseid" m-id-str)
                        (cons "type" "card")))
           (input (drakma:http-request *gatherer-img-url* :parameters query :want-stream t)))
      (awhile (read-byte input nil nil)
        (write-byte it file))
      (close input)
      (format t "Card image ~A successfuly downloaded." image))))

(defun card-exists-p (m-id &key (db *default-database-connection*))
  "Queries the local database to test if the card for Multiverse ID *m-id* is already in the database."
  (postmodern:with-connection db
    (let ((the-card (handler-case (postmodern:get-dao 'rsn-mtg-card (get-card-id-by-multiverse-id m-id)) (error () nil))))
      (if the-card
          t
          nil))))

(defun card-matches-p (m-id card-values &key (db *default-database-connection*))
  "Compares the card DAO in the local database against card-values list parsed from Gatherer to see if all the fields match."
  (let* ((m-id-str (format nil "~D" m-id))
         (the-set-id 0)
         (the-card nil))
    (destructuring-bind (name mana-cost color spell-type collectors-num cmc card-text flavor-text power toughness set-name rarity artist image)
        card-values
      (postmodern:with-connection db
        (setf the-card (handler-case (postmodern:get-dao 'rsn-mtg-card (get-card-id-by-multiverse-id m-id)) (error () nil))
              the-set-id (handler-case (get-expansion-id-by-name set-name) (error () 0))))
      (if (and the-card
               (string-equal (name the-card) name)
               (string-equal (mana-cost the-card) mana-cost)
               (string-equal (color the-card) color)
               (string-equal (spell-type the-card) spell-type)
               (eql (collectors-number the-card) collectors-num)
               (eql (converted-mana-cost the-card) cmc)
               (string-equal (card-text the-card) card-text)
               (string-equal (flavor-text the-card) flavor-text)
               (eql (power the-card) power)
               (eql (toughness the-card) toughness)
               (eql (expansion-id the-card) the-set-id)
               (string-equal (rarity the-card) rarity)
               (string-equal (artist the-card) artist)
               (string-equal (image the-card) image)
               )
          t
          nil))))

;; Rewrite this function as a member-of set test
;; needs a precompiled list of valid M-IDs, tested using the current following code
(defun valid-m-id-p (card-values)
  "Tests the parsed card data from Gatherer database for validity."
  (let ((card-name (car card-values)))
    (if (string-equal card-name "none")
        nil
        t)))

(defun scrape-gatherer-and-insert-mtg-card-into-db (m-id card-values &key (db *default-database-connection*))
  "Query the Gatherer database for the Multiverse ID *m-id* card, parse all data; if the record already exists, update it, or insert it into the local database."
  (destructuring-bind (name mana-cost color spell-type collectors-num converted-mana-cost card-text flavor-text power toughness set-name rarity artist image)
      card-values
    (let* ((full-image-path (concatenate 'string (namestring *default-img-dir*) image))
           (the-set-id (handler-case
                           (postmodern:with-connection db
                             (get-expansion-id-by-name set-name))
                         (error ()
                           (format t "~%;; ~A expansion not found in local database. Creating..." set-name)
                           (postmodern:with-connection db
                             (let* ((set-name-formatted (string-downcase (substitute #\- #\Space set-name)))
                                    (exp-symb (format nil "set-~A" set-name-formatted))
                                    (unc-symb (format nil "set-~A-uncommon" set-name-formatted))
                                    (rare-symb (format nil "set-~A-rare" set-name-formatted))
                                    (myth-symb (format nil "set-~A-mythic" set-name-formatted))
                                    (the-set (postmodern:make-dao 'rsn-mtg-expansion :name set-name :exp-symbol exp-symb :uncommon-symbol unc-symb :rare-symbol rare-symb :mythic-symbol myth-symb :total-cards 0)))
                               (format t "~C[32;1m~C~C[0m" #\Escape (code-char #x2714) #\Escape)
                               (id the-set))))))
           (the-card nil))
      (if (card-exists-p m-id :db db)
          (handler-case
              (postmodern:with-connection db
                (format t "~%;; Updating record for Multiverse ID: ~D, ~A. " m-id name)
                (setf the-card (postmodern:get-dao 'rsn-mtg-card (get-card-id-by-multiverse-id m-id)))
                ;; Update the slot values of the-card, and send to local database
                (setf (name the-card) name
                      (mana-cost the-card) mana-cost
                      (color the-card) color
                      (spell-type the-card) spell-type
                      (collectors-number the-card) collectors-num
                      (converted-mana-cost the-card) converted-mana-cost
                      (card-text the-card) card-text
                      (flavor-text the-card) flavor-text
                      (power the-card) power
                      (toughness the-card) toughness
                      (expansion-id the-card) the-set-id
                      (rarity the-card) rarity
                      (artist the-card) artist
                      (image the-card) image)
                (postmodern:update-dao the-card)
                ;; Need a function to check if the image file has changed or not before redownloading...
                ; (download-and-save-card-image m-id full-image-path)
                (format t "~C[32;1m~C~C[0m" #\Escape (code-char #x2714) #\Escape))
            (error () (format t "~C[31;1m~C~C[0m" #\Escape (code-char #x2718) #\Escape)))
          (progn
          ;(handler-case;)
              (postmodern:with-connection db
                (format t "~%;; Inserting record for Multiverse ID: ~D, ~A. Set-ID: ~D. " m-id name the-set-id)
                (setf the-card (make-instance 'rsn-mtg-card :m-id m-id :name name :mana-cost mana-cost
                                                            :color color :spell-type spell-type :collectors-number collectors-num
                                                            :converted-mana-cost converted-mana-cost :card-text card-text :flavor-text flavor-text
                                                            :power power :toughness toughness :expansion-id the-set-id
                                                            :rarity rarity :artist artist :image image))
                ; (format t "DAO instantiated: ~S " the-card)
                (postmodern:save-dao the-card)
                ; (format t "Insert successful~A" ". ")
                (unless (probe-file full-image-path)
                  (download-and-save-card-image m-id full-image-path))
                (format t "~C[32;1m~C~C[0m" #\Escape (code-char #x2714) #\Escape))
            ;(error () (format t "~C[31;1m~C~C[0m" #\Escape (code-char #x2718) #\Escape))
            ))
      )))

;; Remove, redundant
; (defun scrape-gatherer-and-update-mtg-card-in-db (m-id &key (db *default-database-connection*))
;   "Update an existing local database record for Multiverse ID *m-id* card from Gatherer. Triggers a condition if DAO for *m-id* does not exist."
;   (format t "~%;; Updating Multiverse ID: ~D DAO record... " m-id)
;   (format t "~C[32;1m~C~C[0m" #\Escape (code-char #x2714) #\Escape))

(defun sync-db-from-gatherer (&key (db *default-database-connection*) (update-p nil) (verbose nil) (start 0) (end 380515))
  "Calls the web-scraper to search the official Gatherer DB, and builds DAOs for each object not in the local database."
  (format t "~%;; Syncing local database ~{~A~} from Gatherer..." (last db))
  (loop for i from start below end ;; set this number low for testing; as of Journey into Nyx, Multiverse IDs go up to 380,515
        do (let ((m-id (+ i 1)))
             (if (and (card-exists-p m-id :db db)
                      (not update-p))
                 (if verbose
                     (format t "~%;; Multiverse ID: ~D found in local database. Skipping..." m-id)
                     (progn
                       (format t "~C" #\.)
                       (finish-output)))
                 (let* ((m-id-str (format nil "~D" m-id))
                        (query (list (cons "multiverseid" m-id-str)))
                        (input (drakma:http-request *gatherer-card-url* :parameters query))
                        (document (chtml:parse input (cxml-stp:make-builder)))
                        (parsed-card-values (parse-card m-id document)))
                   (cond ((not m-id) nil)
                         ((not (valid-m-id-p parsed-card-values))
                          (if verbose
                              (format t "~%;; Multiverse ID: ~D does not specify a valid MTG Card. Skipping..." m-id)
                              (progn
                                (format t "~C" #\.)
                                (finish-output))))
                         ((and (card-exists-p m-id :db db)
                               (card-matches-p m-id parsed-card-values :db db))
                          (if verbose
                              (format t "~%;; Multiverse ID: ~D found in local database and matches Gatherer. Skipping..." m-id)
                              (progn
                                (format t "~C" #\.)
                                (finish-output))))
                         ((valid-m-id-p parsed-card-values)
                          (scrape-gatherer-and-insert-mtg-card-into-db m-id parsed-card-values :db db))
                         (t (loop-finish)))))))
  (format t "~%;; Updating card counts for Expansions...")
  (postmodern:with-connection db
    (let ((expansion-sets (postmodern:query
                            (:select '* :from 'rsn-mtg-expansion)
                            (:dao rsn-mtg-expansion))))
      (loop for set in expansion-sets
            do (let* ((set-id (id set))
                      (set-name (name set))
                      (count (postmodern:query (:select (:count '*) :from 'rsn-mtg-card :where (:= 'expansion-id set-id)) :single)))
                 (when (/= (total-cards set) count)
                   (setf (total-cards set) count)
                   (postmodern:update-dao set))
                 (format t "~%;; ~A: ~D Cards in local database" set-name count)))))
  (format t "~%;; Sync Complete."))

;; EOF
