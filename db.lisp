;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RSN-MTG; Base: 10 -*- file: db.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :rsn-mtg)

;; EXPANSION SET Table
(defclass rsn-mtg-expansion ()
  ((id :col-type serial :initarg :id :reader id)
   (name :col-type string :initarg :name :accessor name)
   (symbol :col-type string :initarg :symbol :accessor symbol)
   (uncommon-symbol :col-type string :initarg :uncommon-symbol :accessor uncommon-symbol)
   (rare-symbol :col-type string :initarg :rare-symbol :accessor rare-symbol)
   (mythic-symbol :col-type string :initarg :mythic-symbol :accessor mythic-symbol)
   (total-cards :col-type integer :initarg :total-cards :accessor total-cards))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-mtg-expansion
  (!dao-def))

(defprepared get-expansion-id-by-name
  (:select 'id :from 'rsn-mtg-expansion :where (:= 'name '$1))
  :single!)

;; CARD Table
(defclass rsn-mtg-card ()
  ((id :col-type serial :initarg :id :reader id)
   (name :col-type string :initarg :name :accessor name)
   (mana-cost :col-type string :initarg :mana-cost :accessor mana-cost)
   (color :col-type string :initarg :color :accessor color)
   (spell-type :col-type string :initarg :spell-type :accessor spell-type)
   (collectors-number :col-type integer :initarg :collectors-number :accessor collectors-number)
   (converted-mana-cost :col-type integer :initarg :converted-mana-cost :accessor converted-mana-cost)
   (card-text :col-type string :initarg :card-text :accessor card-text)
   (flavor-text :col-type string :initarg :flavor-text :accessor flavor-text)
   (power :col-type integer :initarg :power :accessor power)
   (toughness :col-type integer :initarg :toughness :accessor toughness)
   (expansion-id :col-type integer :initarg :expansion-id :accessor expansion-id)
   (rarity :col-type string :initarg :rarity :accessor rarity)
   (artist :col-type string :initarg :artist :accessor artist)
   (image :col-type string :initarg :image :accessor image))
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

;; CARD QUANTITY Table

;; CARD VALUE Table

;; DECK Table

;; PLAYER PROFILE Table

;; EOF
