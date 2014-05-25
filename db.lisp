;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: RSN-MTG; Base: 10 -*- file: db.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :rsn-mtg)

(defvar *default-database-connection* (list "database" "user" "password" "host"))

;; PLAYER PROFILE Table

(defclass rsn-mtg-player-profile ()
  ((ID :col-type serial :initarg :id :reader id)
   (USER-ID :col-type integer :initarg :user-id :accessor user-id)
   (ACCT-TYPE :col-type string :initarg :acct-type :accessor acct-type)
   (ACCT-NAME :col-type string :initarg :acct-name :accessor acct-name)
   (CREATED-ON :col-type timestamp :initarg :created-on :reader created-on)
   (LAST-MODIFIED :col-type timestamp :initarg :last-modified :accessor last-modified))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-mtg-player-profile
  (!dao-def)
  (!foreign 'rsn-auth-user 'user-id :primary-key :on-delete :cascade :on-update :cascade))

(defgeneric user (player))

(defmethod user ((player rsn-mtg-player-profile))
  (let ((user-id (slot-value player 'user-id)))
    (postmodern:get-dao 'rsn-auth-user user-id)))

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
  :single)

(defgeneric expansion (card)
  (:documentation "Get the Set/Expansion DAO for the given card."))

(defmethod expansion ((card rsn-mtg-card))
  (let ((exp-id (slot-value card 'expansion-id)))
    (postmodern:get-dao 'rsn-mtg-expansion exp-id)))

;; CARD QUANTITY Table

(defclass rsn-mtg-quantity ()
  ((ID :col-type serial :initarg :id :reader id)
   (CARD-ID :col-type integer :initarg :card-id :accessor card-id)
   (PLAYER-ID :col-type integer :initarg :player-id :accessor player-id)
   (IS-FOIL :col-type boolean :initarg :is-foil :accessor is-foil)
   (CARD-CONDITION :col-type string :initarg :card-condition :accessor card-condition)
   (CARD-COST :col-type string :initarg :card-cost :accessor card-cost)
   (CARD-ORIGIN :col-type string :initarg :card-origin :accessor card-origin))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-mtg-quantity
  (!dao-def)
  (!foreign 'rsn-mtg-card 'card-id :primary-key :on-delete :cascade :on-update :cascade)
  (!foreign 'rsn-auth-player-profile 'player-id :primary-key :on-delete :cascade :on-update :cascade))

(defgeneric card (dao))

(defmethod card ((dao rsn-mtg-quantity))
  (let ((card-id (slot-value dao 'card-id)))
    (postmodern:get-dao 'rsn-mtg-card card-id)))

(defgeneric player (dao))

(defmethod player ((dao rsn-mtg-quantity))
  (let ((player-id (slot-value dao 'player-id)))
    (postmodern:get-dao 'rsn-mtg-player-profile player-id)))

(defgeneric reg-quantity (card player))

(defmethod reg-quantity ((card rsn-mtg-card) (player rsn-mtg-player-profile))
  (let ((card-id (slot-value card 'card-id))
        (player-id (slot-value player 'player-id)))
    (postmodern:query (:select (:count '*) :from 'rsn-mtg-quantity
                               :where (:and (:= 'card-id card-id)
                                            (:= 'player-id player-id)
                                            (:= 'is-foil nil)))
                      :single)))

(defgeneric foil-quantity (card player))

(defmethod foil-quantity ((card rsn-mtg-card) (player rsn-mtg-player-profile))
  (let ((card-id (slot-value card 'card-id))
        (player-id (slot-value player 'player-id)))
    (postmodern:query (:select (:count '*) :from 'rsn-mtg-quantity
                               :where (:and (:= 'card-id card-id)
                                            (:= 'player-id player-id)
                                            (:= 'is-foil t)))
                      :single)))

(defgeneric total-quantity (card player))

(defmethod total-quantity ((card rsn-mtg-card) (player rsn-mtg-player-profile))
  (let ((card-id (slot-value card 'card-id))
        (player-id (slot-value player 'player-id)))
    (postmodern:query (:select (:count '*) :from 'rsn-mtg-quantity
                               :where (:and (:= 'card-id card-id)
                                            (:= 'player-id player-id)))
                      :single)))

;; CARD VALUE Table

(defclass rsn-mtg-card-value ()
  ((ID :col-type serial :initarg :id :reader id)
   (CARD-ID :col-type integer :initarg :card-id :accessor card-id)
   (HIGH-PRICE :col-type double-float :initarg :high-price :accessor high-price)
   (AVG-PRICE :col-type double-float :initarg :avg-price :accessor avg-price)
   (LOW-PRICE :col-type double-float :initarg :low-price :accessor low-price)
   (AS-OF-DATE :col-type timestamp :initarg :as-of-date :accessor as-of-date))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-mtg-card-value
  (!dao-def)
  (!foreign 'rsn-mtg-card 'card-id :primary-key :on-delete :cascade :on-update :cascade))

(defmethod card ((dao rsn-mtg-card-value))
  (let ((card-id (slot-value dao 'card-id)))
    (postmodern:get-dao 'rsn-mtg-card card-id)))

;; DECK Tables

(defclass rsn-mtg-deck ()
  ((ID :col-type serial :initarg :id :reader id)
   (NAME :col-type string :initarg :name :accessor name)
   (DECK-FORMAT :col-type string :initarg :deck-format :accessor deck-format))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-mtg-deck
  (!dao-def)
  (!foreign))

(defclass rsn-mtg-deck-cards ()
  ((ID :col-type serial :initarg :id :reader id)
   (DECK-ID :col-type integer :initarg :deck-id :accessor deck-id)
   (CARD-ID :col-type integer :initarg :card-id :accessor card-id))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-mtg-deck-cards
  (!dao-def)
  (!foreign 'rsn-mtg-deck 'deck-id :primary-key :on-delete :cascade :on-update :cascade)
  (!foreign 'rsn-mtg-card 'card-id :primary-key :on-delete :cascade :on-update :cascade))

;; EOF
