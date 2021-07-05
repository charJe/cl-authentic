;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; -*-
;;;
;;; tests.lisp --- Password management for Common Lisp (web) applications.

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
(defpackage authentic-tests
  (:use #:cl #:authentic #:fiveam)
  (:shadow #:run))

(in-package #:authentic-tests)

(defun random-username ()
  (format nil "~A" (random 1000000000000000)))

(defparameter *db-spec* '(("authentic-testdb.sqlite3")
			  :database-type :sqlite3 :if-exists :new))
(def-suite tests)

(in-suite tests)

(test default-database-found-or-created
      (is (not (null (open-password-database :spec *db-spec*)))))

(defun kill-kwarg (list kw)
  (loop :for (key val) :on list :by #'cddr
     :unless (eq key kw)
     :collect key :and :collect val))

(test database-really-gone
  (with-password-database ((open-password-database :spec *db-spec*))
    (destructuring-bind (designator &rest kwargs)
	 *db-spec*
      (is-true (apply #'clsql:destroy-database designator
		      (kill-kwarg kwargs :if-exists))))))

(test default-store-created-and-opened
  (with-password-database ((open-password-database :spec *db-spec*))
    (let ((s1 (open-password-store))
	  (s2 (open-password-store)))
      (is (not (null s1)))
      (is (not (null s2))))))

(test unknown-user-does-not-exist
  (with-password-database ((open-password-database :spec *db-spec*))
    (with-password-store ((open-password-store))
      (is (null (user-knownp (random-username)))))))

(test authenticate-unknown-must-fail
  (with-password-database ((open-password-database :spec *db-spec*))
    (with-password-store ((open-password-store))
      (signals user-unknown
	(password-store:authenticate-user (random-username) "foo")))))

(test register-new-user-1
  (with-password-database ((open-password-database :spec *db-spec*))
    (with-password-store ((open-password-store))
      (let ((user (random-username)))
	(is (string= 
	     user
	     (register-user user :needs-confirmation-within nil
			    :password "foo")))))))

(test register-new-user-1-and-auth
  (with-password-database ((open-password-database :spec *db-spec*))
    (with-password-store ((open-password-store))
      (let ((user (random-username)))
	(register-user user :needs-confirmation-within nil
		       :password "foo")
	;; since we require no confirmation the account is active
	(is-true (authenticate-user user "foo"))
	(is-false (authenticate-user user "Foo"))))))

(test have-2-users-now
  (with-password-database ((open-password-database :spec *db-spec*))
    (with-password-store ((open-password-store))
      (is (= 2 (length (all-users)))))))
