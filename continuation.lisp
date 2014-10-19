#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *continuation-life* (* 60 60 24))

(defclass continuation ()
  ((id :initarg :id :initform NIL :accessor id)
   (timeout :initarg :timeout :initform (+ (get-universal-time) *continuation-life*) :accessor timeout)
   (continued-request :initarg :request :initform (error "REQUEST required.") :accessor continued-request)
   (handler :initarg :handler :initform (error "HANDLER required.") :accessor handler)))
