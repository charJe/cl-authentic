;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; -*-
;;;
;;; make-markdown-tinaa-doc.lisp --- Documentation generator.

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

;; This is a gross hack but largely works (except that I did not bother
;; to fix the :before method on maybe-show-documentation that 
;; switches internal/external symbols on and off. TINAA also seems
;; to incorrectly handle slots and accessors of conditions.
;;
;; Load this in a lisp session to regenerate documentation under #p"doc/"
;;

(in-package :cl-user)

(ql:quickload "external-program")
(asdf:operate 'asdf:load-op "authentic")

(defparameter *markdown-processor* "/usr/bin/markdown2"
  "Markdown binary to call.")

(defparameter *process-markdown* T
  "Whether to actually process markdown in maybe-process-markdown.")

(defun maybe-process-markdown (docstring)
  "Transform docstring into html-body, if *PROCESS-MARKDOWN* is non-nil, 
otherwise the identity."
  (if (and *process-markdown* (stringp docstring))
      (with-output-to-string (result)
	(with-input-from-string (s docstring)
	  (external-program:run *markdown-processor* '() :input s :output result)))
      docstring))


;; hook into tinaa
(defmethod tinaa::short-documentation :around (part)
  ;; short documentation gets extracted from the raw stuff
  (let ((*process-markdown* nil))
    (call-next-method)))

(defmethod tinaa:part-documentation :around (part)
  ;; markdownize docstring
  (maybe-process-markdown (call-next-method)))

(defmethod tinaa::maybe-show-documentation :around (part)
  (if *process-markdown*
      (let ((documentation (tinaa:part-documentation part)))
	(when documentation 
	  (lml2:html ((:div :class "documentation") 
		      (lml2:lml-princ documentation)))))
      (call-next-method)))

(tinaa:document-system 'package 'authentic #p"gh-pages/")
