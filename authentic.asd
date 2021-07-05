;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; -*-
;;;
;;; authentic.asd --- Password management for Common Lisp (web) applications.

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
(defpackage authentic-system
  (:use #:cl #:asdf))

(in-package :authentic-system)

(defsystem #:authentic
  :serial t
  :description "Password management for Common Lisp (web) applications."
  :author "Utz-Uwe Haus <lisp@uuhaus.de>"
  :license "LLGPL"
  :version "0.1.0"
  :depends-on ("clsql" "ironclad")
  :components ((:file "package")
	       (:file "conditions")
               (:file "authentic"))
  :in-order-to ((test-op (load-op authentic-test)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :authentic-tests)
                             (intern "TESTS" :authentic-tests))))
(defmethod operation-done-p
    ((op test-op) (c (eql (find-system :authentic))))
  (values nil))

(defsystem authentic-test
  :depends-on ("authentic" "fiveam")
  :components ((:file "tests")))

