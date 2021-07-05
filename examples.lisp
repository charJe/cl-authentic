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

;;;
;;; A sample session of typical uses
;;;
(in-package :cl-user)
(setq authentic:*default-password-database*
      (authentic:open-password-database))
;; => clsql database object
(setq authentic:*default-password-store*
      (authentic:open-password-store))
;; => a password store object

(authentic:user-knownp "Bob")
;; => NIL
(authentic:authenticate-user "Bob" "foo")
;; => condition: user-unknown
(authentic:register-user "Bob" :needs-confirmation-within nil
				 :password "foo")
;; => "Bob"
(authentic:all-users)
;; => ("Bob")

(authentic:delete-user "Bobby")
;; => condition: user-unknown
(authentic:delete-user "Bobby" :no-exist-ok T)
;; => NIL
(authentic:delete-user "Bob")
;; => "Bob"
(authentic:all-users)
;; => NIL

(authentic:register-user "Bob" :needs-confirmation-within nil
				 :password "foo")
;; => "Bob"
(authentic:authenticate-user "Bob" "fooness")
;; => NIL
(authentic:authenticate-user "Bob" "foo")

;;
(authentic:register-user "Alice" 
				 :needs-confirmation-within 
				 (clsql:make-duration) ;; i.e. 0 sec.
				 :password "foo")
;; "Alice" , ...long numeric string...
(authentic:authenticate-user "Alice" "foo")
;; => NIL 
(multiple-value-bind (name confirmation-token)
    (authentic:register-user "Frank" 
				     :needs-confirmation-within 
				     (clsql:make-duration) ;; i.e. 0 sec.
				     :password "foo")
  (authentic:confirm-registration name "42")
  ;; => NIL
  (authentic:confirm-registration name confirmation-token)
  ;; => condition: confirmation-token-expired
  )

(multiple-value-bind (name confirmation-token)
    (authentic:register-user "Frank" 
				     :needs-confirmation-within 
				     (clsql:make-duration) ;; i.e. 0 sec.
				     :password "foo")
  (authentic:confirm-registration name "42")
  ;; => NIL
  (authentic:confirm-registration name confirmation-token)
  ;; => condition: confirmation-token-expired
  )
(let ((new-token
       (authentic:get-user-confirmation-token 
	"Frank" :validity-duration (clsql:make-duration :day 1))))
  (authentic:confirm-registration "Frank" new-token))
;; => T

