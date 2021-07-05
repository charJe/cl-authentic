;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; -*-
;;;
;;; conditions.lisp --- Password management for Common Lisp (web) applications.

;; Copyright (C) 2013 Utz-Uwe Haus <lisp@uuhaus.de>
;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the version 2 of the GNU Library General
;; Public License as published by the Free Software Foundation, as
;; clarified by the prequel found in LICENSE.LLGPL
;;

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, 
;; Boston, MA  02110-1301  USA
;;
;; Commentary:
;; 
(in-package #:authentic)

(define-condition authentic-condition ()
  ()
  (:documentation "Superclass of all conditions raised by authentic."))

(define-condition user-exists (authentic-condition)
  ((user-token :accessor get-user-token :initarg :user-token
	       :documentation "The user-token that triggered the condition."))
  (:report
   (lambda (condition stream)
     (format stream "User ~A already exists." (get-user-token condition))))
  (:documentation
   "Condition raised if an attempt was made to create a user that already exists."))

(define-condition user-unknown (authentic-condition)
  ((user-token :accessor get-user-token :initarg :user-token
	       :documentation "The user-token that triggered the condition."))
  (:report
   (lambda (condition stream)
     (format stream "User ~A does not exist." (get-user-token condition))))
  (:documentation 
   "Condition raised if an operation was attempted with an unknown user-token."))

(define-condition password-token-expired (authentic-condition)
  ((user-token :accessor get-user-token :initarg :user-token
	       :documentation "The user-token that triggered the condition.")
   (expiry     :accessor get-expiry     :initarg :expiry
	       :documentation "The expiration time of password change token."))
  (:report
   (lambda (condition stream)
     (format stream "User ~A's password reset token expired ~A."
	     (get-user-token condition)
	     (clsql:format-time nil (get-expiry condition)))))
  (:documentation
   "Condition raised if a password change was attempted with a token whose validity duration has expired."))

(define-condition password-token-invalid (authentic-condition)
  ((user-token :accessor get-user-token :initarg :user-token
	       :documentation "The user-token that triggered the condition.")
   (reset-token :accessor get-reset-token     :initarg :reset-token
	       :documentation "The token that is invalid."))
  (:report
   (lambda (condition stream)
     (format stream "User ~A's password reset token is invalid: ~A."
	     (get-user-token condition)
	     (get-reset-token condition))))
  (:documentation
   "Condition raised if a password change was attempted with a bad token"))

(define-condition confirmation-token-expired (authentic-condition)
  ((user-token :accessor get-user-token :initarg :user-token
	       :documentation "The user-token that triggered the condition.")
   (expiry     :accessor get-expiry     :initarg :expiry
	       :documentation "The expiration time of password change token."))
  (:report
   (lambda (condition stream)
     (format stream "User ~A's confirmation token expired ~A."
	     (get-user-token condition)
	     (clsql:format-time nil (get-expiry condition)))))
  (:documentation 
   "Condition raised if a user confirmation was attempted with a token whose validity duration has expired."))

(define-condition confirmation-token-invalid (authentic-condition)
  ((user-token :accessor get-user-token :initarg :user-token
	       :documentation "The user-token that triggered the condition.")
   (confirmation-token :accessor get-confirmation-token     :initarg :confirmation-token
		       :documentation "The token that is invalid."))
  (:report
   (lambda (condition stream)
     (format stream "User ~A's confirmation token is invalid: ~A."
	     (get-user-token condition)
	     (get-confirmation-token condition))))
  (:documentation
   "Condition raised if a user confirmation was attempted with a bad token"))



