;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; -*-
;;;
;;; cl-password-store.lisp --- Password management for Common Lisp (web) applications.

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

(in-package #:cl-password-store)
(clsql:file-enable-sql-reader-syntax)

(defvar *default-password-database* clsql:*default-database*
  "The default password database.")
(defvar *default-password-hash* :sha256
  "The default password hash used.")
(defvar *default-password-store* nil
  "The default password store to use.")
(defvar *default-password-store-pepper* "42"
  "The pepper value for hashing. Please change in your application.)")
(defvar *default-token-validity* (clsql:make-duration :day 1)
  "The default duration that a token is valid.")


;;; Tokens
(defconstant +token-bits+ 129
  "Number of bits to use in a random token.")

(defun generate-token ()
  "Generate a random string suitable as a token."
  (format nil "~A" (random (expt 2 +token-bits+))))

(defun token-equal (t1 t2)
  "Equality predicate for tokens."
  ;; since hashes are hex-strings limited by CLSLQ 'string' fields:
  (string= t1 t2
	   :end1 (min (length t1) clsql:*default-string-length*)
	   :end2 (min (length t2) clsql:*default-string-length*)))

;;; The password store
(defclass password-store ()
  ((db              :accessor get-db
		    :initarg :db
		    :type clsql:database
		    :documentation "The database used by this store.")
   (hash            :accessor get-hash
		    :initarg :hash
		    :documentation "The type of hash-digest to be used.")
   (pepper          :reader get-pepper :initarg :pepper
	            :type string
	            :documentation "Pepper value used in hashing.")
   (view-class-name :accessor get-view-class-name 
		    :initarg :view-class-name
		    :documentation 
		    "Name of the view class for password entries, a subclass of PASSWORD-ENTRY."))
  (:documentation "A password store.")
  (:default-initargs
      :view-class-name 'password-entry
    :db *default-password-database*
    :hash *default-password-hash*
    :pepper *default-password-store-pepper*))


;;; The password entry class and table

;; some minor preparations for schema migration here:
;; the current password-entry always has an explicitly versioned
;; slq table name. That way we can notice old versions floating
;; around in a database and offer to migrate the data.
(clsql:def-view-class password-entry ()
  ((user-token          :db-kind :key
			:db-constraint :not-null
			:type string
			:initarg :user-token
			:reader get-user-token
			:documentation
			"A unique name for the user, as a string of up to
 `clsql:*default-string-length*` characters.")
   (salt               :type string :initarg :salt
		       :accessor get-salt
		       :documentation "Salt value for this entry.")
   (hashed-password    :type string
		       :initarg :password-hash
		       :accessor get-hashed-password
		       :documentation "The hashed password.")
   (reset-token        :type string :accessor get-reset-token
		       :documentation "If we are in a password reset phase, the token that needs to be presented to change the password, `NIL` otherwise.")
   (token-expiry       :type clsql:wall-time :accessor get-token-expiry
		       :documentation "Date when reset-token expires.")
   (confirmation-token :type string :accessor get-confirmation-token
		       :initarg :confirmation-token
		       :documentation "If we are in account creation phase, the token that needs to be presented to activate the account, `NIL` otherwise.")
   (confirmation-token-expiry
                       :type clsql:wall-time
		       :accessor get-confirmation-token-expiry
		       :initarg :confirmation-token-expiry
		       :documentation "Date when confirmation-token expires."))
  (:documentation "View class used to represent a user/password entry and map it to SQL table data.")
  (:base-table PASSWORD_ENTRY_20130712))

(defgeneric password-entry-schema-migration-handler (database view-class-name)
  (:documentation "A function to handle schema migration for password entry table.
Takes two arguments, the `clsql:database` object and the current view class, e.g. [`password-entry`](class-password--entry.html).")
  (:method (database view-class-name)
    (declare (ignore database view-class-name))
    (values)))

(defun ensure-password-table (&key
				(database *default-password-database*)
				(view-class-name 'password-entry)
				(schema-migration-handler
				 #'password-entry-schema-migration-handler))
  "Ensure that DATABASE has a suitable password table named TABLENAME."
  (unless (clsql:table-exists-p (clsql-sys:view-table 
				 (find-class view-class-name))
	   :database database)
    ;; table needs to be created
    (clsql:create-view-from-class view-class-name
				  :database database :transactions T))
  (when schema-migration-handler
    ;; let migration handler kick in
    (funcall schema-migration-handler database view-class-name)))

(defun generate-salt (token)
  "Generate a salt value for password hashing on TOKEN. Returns a string."
  (format nil "~A~D" token (random 1000000)))

(defun find-user (user-token store)
  "Find user USER-TOKEN in STORE or signal a USER-UNKNOWN condition."
  (or (user-knownp user-token :store store)
      (error (make-condition 'user-unknown :user-token user-token))))

(defgeneric add-user (store token password needs-confirmation-within)
  (:documentation "Low level user creation function.
Add user identified by TOKEN to STORE, with PASSWORD set, and possibly lock the account by requiring confirmation, depending on NEEDS-CONFIRMATION-WITHIN duration value.")
  (:method ((store password-store) token password needs-confirmation-within)
    (let* ((salt (generate-salt token))
	   (record (make-instance
		    (get-view-class-name store)
		    :user-token token
		    :salt salt
		    :password-hash (compute-password-hash store password salt)
		    :confirmation-token (and needs-confirmation-within
					     (generate-token))
		    :confirmation-token-expiry 
		    (and needs-confirmation-within
			 (compute-expiration-date needs-confirmation-within)))))
      (clsql:update-records-from-instance record :database (get-db store))
      token)))

(defun compute-expiration-date (validity)
  "Compute the expiration date, which is at VALIDITY after current time."
  (clsql:time+ (clsql:get-time) validity))


;;; hashing
(defgeneric compute-password-hash (store password salt)
  (:documentation "Compute the appropriate hash value for PASSWORD in STORE.")
  (:method ((store password-store) (password (eql nil)) salt)
    (declare (ignore salt))
    NIL)
  (:method ((store password-store) (password string) (salt string))
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence 
      (get-hash store)
      (ironclad:ascii-string-to-byte-array
       (concatenate 'string (get-pepper store)
		    password salt))))))

(defun password-hash-equal (p1 p2)
  "Compare P1 and P2 for equality."
  ;; since hashes are hex-strings limited by CLSLQ 'string' fields:
  (string= p1 p2 
	   :end1 (min (length p1) clsql:*default-string-length*)
	   :end2 (min (length p2) clsql:*default-string-length*)))


;;; public API
;;; user tokens can simply be strings, or you can use a class derived
;;; from user-token-mixin and rely on this code to call user-token-id
(deftype clsql-default-string-length () `(string ,clsql:*default-string-length*))
(defclass user-token-mixin ()
  ((id :type clsql-default-string-length
       :accessor get-id :initarg :id
       :documentation "A string identifying the user."))
  (:documentation "Mixin class for objects that can serve as user-token."))

(defgeneric user-token-id (user-token)
  (:documentation 
   "A function returning a string identifier of at most 255 characters suitable to identify each user")
  (:method ((token user-token-mixin))
    (get-id token))
  (:method ((token string))
    token))

(defun open-password-database 
    (&key (spec '(("cl-password-store.sqlite3") :database-type :sqlite3 :if-exists :old)))
  "Open password database specified by :SPEC, which must be an argument list suitable for clsql:connect.
Default is to use '((\"cl-password-store.sqlite3\") :sqlite3); if you use this default you will have to have clsql-sqlite3 loaded too.
Returns an clsql:database object suitable as argument to OPEN-PASSWORD-STORE."
  (apply #'clsql:connect spec))

(defun open-password-store
    (&key
       (database *default-password-database*)
       (view-class-name 'password-entry)
       (hash *default-password-hash*)
       (schema-migration-handler #'password-entry-schema-migration-handler)
       (pepper *default-password-store-pepper*))
  "Open password store designated by TABLENAME in DATABASE (default: *default-password-database*)"
  (unless (clsql:find-database database)
    (error "Database ~A is not usable" database))
  (ensure-password-table :database database :view-class-name view-class-name
			 :schema-migration-handler schema-migration-handler)
  (make-instance 'password-store 
		 :db database :view-class-name view-class-name :hash hash
		 :pepper pepper))

(defgeneric register-user
    (user-token &key store password needs-confirmation-within)
  (:documentation
   "Register user identified by USER-TOKEN in store specified by :STORE argument (default: *default-password-store*). Set password if :PASSWORD is given (default: no password set, meaning locked account). :NEEDS-CONFIRMATION-within (default *default-token-validity*) makes the account unusable until confirmation has occured. Can be NIL to indicate that no confirmation is needed.
Returns up to two values: a user object and possibly the confirmation token.
raises a user-exists condition if user already exists.")
  (:method ((user-token user-token-mixin)
	    &key (store *default-password-store*)
	      password
	      (needs-confirmation-within *default-token-validity*))
    (register-user (user-token-id user-token) 
		   :store store :password password
		   :needs-confirmation-within needs-confirmation-within))
  (:method ((user-token string)
	    &key (store *default-password-store*)
	      password
	      (needs-confirmation-within *default-token-validity*))
    (if (user-knownp user-token :store store)
	(error (make-condition 'user-exists :user-token user-token))
	;; FIXME: registration confirmation tokens missing
	(let ((record (add-user store user-token password
				needs-confirmation-within)))
	  ;; we have add-user setting the confirmation token
	  ;; to avoid the tiny race condition of having an account
	  ;; without confirmation token authenticable before the confirmation
	  ;; token is set through the normal confirmation request API.
	  (if needs-confirmation-within
	      (values record 
		      (get-confirmation-token (user-knownp user-token :store store)))
	      (values record))))))

(defgeneric get-user-confirmation-token
    (user-token &key store validity-duration)
  (:documentation
   "Create a new account confirmation token, register it for USER-TOKEN (replacing a possibly-existing old one) in STORE (default: *default-password-store*), and note that it will after VALIDITY-DURATION (default: *default-token-validity*), a duration as created by clsql:make-duration. 
Account cannot be authenticated until confirm-registration was successful.
Returns a string, the token.")
  (:method ((user-token user-token-mixin)
	    &key (store *default-password-store*)
	      (validity-duration *default-token-validity*))
    (get-user-confirmation-token 
     (user-token-id user-token)
     :store store :validity-duration validity-duration))
  (:method ((user-token string)
	    &key (store *default-password-store*)
	      (validity-duration *default-token-validity*))
    (let ((record (find-user user-token store)))
      (with-slots (confirmation-token confirmation-token-expiry) 
	  record
	(setf confirmation-token
	      (generate-token)
	      confirmation-token-expiry
	      (compute-expiration-date validity-duration)))
      (clsql:update-records-from-instance record :database (get-db store))
      (get-confirmation-token record))))

(defgeneric confirm-registration
    (user-token confirmation-token &key store)
  (:documentation
   "Confirm USER-TOKEN using CONFIRMATION-TOKEN in STORE (default: *default-password-store*).
Returns a generalized Boolean.")
  (:method ((user-token user-token-mixin) (confirmation-token string) 
	    &key (store *default-password-store*))
    (confirm-registration (user-token-id user-token) confirmation-token
			  :store store))
  (:method ((user-token string) (confirmation-token string) 
	    &key (store *default-password-store*))
    (let ((record (or (user-knownp user-token :store store)
		      (error (make-condition 'user-unknown
					     :user-token user-token)))))
      (cond
	((not (get-confirmation-token record))
	 (error (make-condition 'confirmation-token-invalid :user-token user-token
				:confirmation-token confirmation-token)))
	((not (clsql:time< (clsql:get-time)
			   (get-confirmation-token-expiry record)))
	 (error (make-condition
		 'confirmation-token-expired
		 :user-token user-token
		 :expiry (get-confirmation-token-expiry record))))
	((token-equal (get-confirmation-token record) confirmation-token)
	 (setf (get-confirmation-token record) nil
	       (get-confirmation-token-expiry record) nil)
	 (clsql:update-records-from-instance record :database (get-db store))
	 T)
	(T
	 NIL)))))

(defgeneric pending-confirmationp (password-entry &key store)
  (:documentation "Check whether PASSWORD-ENTRY is blocked because it needs confirmation.
Returns a generalized Boolean.")
  (:method ((record password-entry) &key (store *default-password-store*))
    (declare (ignore store))
    (not (null (get-confirmation-token record))))
  (:method ((user-token string) &key (store *default-password-store*))
    (pending-confirmationp (find-user user-token store) :store store))
  (:method ((user-token user-token-mixin) &key (store *default-password-store*))
    (pending-confirmationp (find-user user-token store) :store store)))

(defgeneric user-knownp
    (user-token &key store)
  (:documentation
   "Check whether user identified by USER-TOKEN is known in store specified by :STORE (default: *default-password-store*).
Returns a password-entry instance or NIL.")
  (:method ((user-token user-token-mixin)
	    &key (store *default-password-store*))
    (user-knownp (user-token-id user-token) :store store))
  (:method ((user-token string) &key (store *default-password-store*))
    (car
     (clsql:select
      (get-view-class-name store)
      :flatp T
	:database (get-db store)
	:where [= user-token 
	[slot-value (get-view-class-name store) 'user-token]]))))

(defgeneric authenticate-user
    (user-token password 
     &key store)
  (:documentation
   "Check whether USER-TOKEN successfully authenticates with
PASSWORD in STORE (default: *default-password-store*).
Returns a generalized Boolean.
If user had a password-reset-token pending, clear it upon successfull auth.")
  (:method ((user-token user-token-mixin) (password string)
	    &key (store *default-password-store*))
    (authenticate-user (user-token-id user-token)
		       password :store store))
  (:method ((user-token string) (password string)
	    &key (store *default-password-store*))
    (let ((record (find-user user-token store)))
      (and record
	   (not (pending-confirmationp record :store store))
	   (let ((authenticated
		  (password-hash-equal 
		   (get-hashed-password record)
		   (compute-password-hash store password (get-salt record)))))
	     (when authenticated
	       (clear-password-reset-token record :store store))
	     authenticated)))))


(defgeneric all-users (&key store)
  (:documentation "Return a list of all user-tokens present in STORE (default: *default-password-store*.")
  (:method (&key (store *default-password-store*))
    (mapcar #'get-user-token
	    (clsql:select (get-view-class-name store)
			  :flatp T
			  :database (get-db store)))))

(defgeneric get-password-reset-token
    (user-token &key store validity-duration)
  (:documentation
   "Create a new password reset token, register it for USER-TOKEN (replacing a possibly-existing old one) in STORE (default: *default-password-store*), and note that it will after VALIDITY-DURATION (default: *default-token-validity*), a duration as created by clsql:make-duration. 
Returns a string, the token.")
  (:method ((user-token user-token-mixin)
	    &key (store *default-password-store*)
	      (validity-duration *default-token-validity*))
    (get-password-reset-token (user-token-id user-token)
			      :store store
			      :validity-duration validity-duration))
  (:method ((user-token string)
	    &key (store *default-password-store*)
	      (validity-duration *default-token-validity*))
    (let ((record (find-user user-token store)))
      (with-slots (reset-token token-expiry) 
	  record
	  (setf reset-token (generate-token)
		token-expiry (compute-expiration-date validity-duration)))
      (clsql:update-records-from-instance record :database (get-db store))
      (get-reset-token record))))

(defgeneric clear-password-reset-token (user &key store)
  (:documentation "Clear password reset token of USER-TOKEN")
  (:method ((user-record password-entry) &key (store *default-password-store*))
    (setf (get-reset-token user-record) NIL
	  (get-token-expiry user-record) NIL)
    (clsql:update-records-from-instance user-record :database (get-db store))
    user-record)
  (:method ((user-token string) &key (store *default-password-store*))
    (clear-password-reset-token (find-user user-token store) :store store))
  (:method ((user-token user-token-mixin) &key (store *default-password-store*))
    (clear-password-reset-token (find-user (user-token-id user-token) store) :store store)))


(defgeneric reset-password
    (user-token reset-token new-password &key store)
  (:documentation
   "Reset password of USER-TOKEN in STORE (default *default-password-store*) to NEW-PASSWORD, authenticating by RESET-TOKEN.
Returns generalized boolean to indicate success.")
  (:method ((user-token user-token-mixin)
	    (reset-token string) (new-password string)
	    &key (store *default-password-store*))
    (reset-password (user-token-id user-token)
		    reset-token new-password
		    :store store))
  (:method ((user-token string)
	    (reset-token string) (new-password string)
	    &key (store *default-password-store*))
    (let ((record (find-user user-token store)))
      (cond 
	((not (get-token-expiry record))
	 (error (make-condition 'password-token-invalid :user-token user-token
				:reset-token reset-token)))
	((not (clsql:time< (clsql:get-time) (get-token-expiry record)))
	 (error (make-condition 'password-token-expired
				:user-token user-token
				:expiry (get-token-expiry record))))
	((token-equal (get-reset-token record) reset-token)
	 (clsql:with-transaction (:database (get-db store))
	   ;; actually change password
	   (setf (get-salt record) 
		 (generate-salt user-token))
	   (setf (get-hashed-password record)
		 (compute-password-hash store new-password (get-salt record)))
	   (clear-password-reset-token record :store store)
	   (clsql:update-records-from-instance
	    record :database (get-db store))))
	(T
	  NIL)))))

(defgeneric delete-user
    (user-token &key store no-exist-ok)
  (:documentation
   "Delete user identified by USER-TOKEN in STORE (default: *default-password-store*). If no-exist-ok is non-nil, silently ignore nonexisting users.")
  (:method ((user-token user-token-mixin)
	    &key (store *default-password-store*) (no-exist-ok nil))
    (delete-user (user-token-id user-token)
		 :store store :no-exist-ok no-exist-ok))
  (:method ((user-token string)
	    &key (store *default-password-store*) (no-exist-ok nil))
    (let ((record (user-knownp user-token :store store)))
      (if record
	  (progn
	    (clsql:delete-instance-records record :database (get-db store))
	    (get-user-token record))
	  (if no-exist-ok
	      NIL
	      (error (make-condition 'user-unknown :user-token user-token)))))))


(defmacro with-password-database
    ((&optional (database *default-password-database*))
     &body body)
  "Evaluate BODY with DATABASE bound to a `clsql:database` (default: `*default-password-database*`)."
  `(let* ((*default-password-database* ,database))
     ,@body))

(defmacro with-password-store
    ((&optional (store *default-password-store*))
     &body body)
  "Evaluate BODY with STORE bound to a [`password-store`](class-password--store.html) (default: `*default-password-store*`)."
  `(let* ((*default-password-store* ,store))
     ,@body))
