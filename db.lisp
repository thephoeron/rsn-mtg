;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RSN-MTG; Base: 10 -*- file: db.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :rsn-mtg)

(defvar *default-database-connection* (list "database" "user" "password" "host"))

;; EXPANSION SET Table
(defclass rsn-mtg-expansion ()
  ((ID :col-type serial :initarg :id :reader id)
   (NAME :col-type string :initarg :name :accessor name)
   (EXP-SYMBOL :col-type string :initarg :exp-symbol :accessor exp-symbol)
   (UNCOMMON-SYMBOL :col-type string :initarg :uncommon-symbol :accessor uncommon-symbol)
   (RARE-SYMBOL :col-type string :initarg :rare-symbol :accessor rare-symbol)
   (MYTHIC-SYMBOL :col-type string :initarg :mythic-symbol :accessor mythic-symbol)
   (TOTAL-CARDS :col-type integer :initarg :total-cards :accessor total-cards))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-mtg-expansion
  (!dao-def))

(defprepared get-expansion-id-by-name
  (:select 'id :from 'rsn-mtg-expansion :where (:ilike 'name '$1))
  :single!)

;; CARD Table
(defclass rsn-mtg-card ()
  ((ID :col-type serial :initarg :id :reader id)
   (M-ID :col-type integer :initarg :m-id :accessor m-id)
   (NAME :col-type string :initarg :name :accessor name)
   (MANA-COST :col-type string :initarg :mana-cost :accessor mana-cost)
   (COLOR :col-type string :initarg :color :accessor color)
   (SPELL-TYPE :col-type string :initarg :spell-type :accessor spell-type)
   (COLLECTORS-NUMBER :col-type integer :initarg :collectors-number :accessor collectors-number)
   (CONVERTED-MANA-COST :col-type integer :initarg :converted-mana-cost :accessor converted-mana-cost)
   (CARD-TEXT :col-type string :initarg :card-text :accessor card-text)
   (FLAVOR-TEXT :col-type string :initarg :flavor-text :accessor flavor-text)
   (POWER :col-type integer :initarg :power :accessor power)
   (TOUGHNESS :col-type integer :initarg :toughness :accessor toughness)
   (EXPANSION-ID :col-type integer :initarg :expansion-id :accessor expansion-id)
   (RARITY :col-type string :initarg :rarity :accessor rarity)
   (ARTIST :col-type string :initarg :artist :accessor artist)
   (IMAGE :col-type string :initarg :image :accessor image))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-mtg-card
  (!dao-def)
  (!foreign 'rsn-mtg-expansion 'expansion-id :primary-key :on-delete :cascade :on-update :cascade))

(defprepared get-card-ids-for-name
  (:select 'id :from 'rsn-mtg-card :where (:= 'name '$1))
  :column)

(defprepared get-card-id-by-name-and-expansion
  (:select 'id :from 'rsn-mtg-card
           :where (:and (:= 'name '$1)
                        (:= 'expansion-id '$2)))
  :single!)

(defprepared get-card-id-by-multiverse-id
  (:select 'id :from 'rsn-mtg-card :where (:= 'm-id '$1))
  :single!)

(defgeneric expansion (card)
  (:documentation "Get the Set/Expansion DAO for the given card."))

(defmethod expansion ((card rsn-mtg-card))
  (let ((exp-id (slot-value card 'expansion-id)))
    (postmodern:get-dao 'rsn-mtg-expansion exp-id)))

;; CARD QUANTITY Table

;; CARD VALUE Table

;; DECK Table

;; PLAYER PROFILE Table

;; EOF
