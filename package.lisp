;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; -*-
;;;
;;; package.lisp --- Password management for Common Lisp (web) applications.

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

(defpackage #:authentic
  (:use #:cl)
  ;; classes:
  (:export #:user-token-mixin
	   #:password-store
	   #:password-entry)
  ;; store creation and database
  (:export #:open-password-database
	   #:open-password-store
	   #:*default-password-database*
	   #:*default-password-hash*
	   #:*default-password-store*
	   #:*default-password-store-pepper*)
  ;; interface of user-token-mixin
  (:export #:user-token-id)
  ;; interface of password-store
  (:export 
   ;; creation
   #:register-user
   #:get-user-confirmation-token
   #:confirm-registration
   #:pending-confirmationp
   ;; query
   #:user-knownp
   #:authenticate-user
   #:all-users
   ;; password reset
   #:get-password-reset-token
   #:reset-password
   ;; deletion
   #:delete-user
   ;; Conditions
   #:user-exists #:user-unknown
   #:password-token-expired #:confirmation-token-expired
   #:password-token-invalid #:confirmation-token-invalid
   ;; convenience macro
   #:with-password-database
   #:with-password-store)
  (:documentation
   "Password management for Common Lisp (web) applications.

`authentic` provides a light-weight and extendible solution to
user/password management:

* safe password storage:
    + cleartext-free, using your choice of hash algorithm through
      [ironclad](http://method-combination.net/lisp/ironclad/),
    + storage in an SQL database through
      [clsql](http://clsql.b9.com/), in a database your application
      already uses anyway, or in a separate one, and using any backend
      supported by clsql,
* password reset mechanism with one-time tokens (suitable for mailing
  to users for confirmation),
* user creation optionally with confirmation tokens (suitable for
  mailing to users),
* (obviously) user authentication.

Users can be identified by strings or by subclassing
[`user-token-mixin`](class-user--token--mixin.html).
"))

