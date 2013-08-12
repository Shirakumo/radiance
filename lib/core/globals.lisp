#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defvar *radiance-startup-time* 0  "Startup time for uptime and such.")
(defvar *radiance-config-file* NIL "Radiance's main JSON configuration file.")
(defvar *radiance-config*      NIL "Radiance's main static configuration.")
(defvar *radiance-acceptors*   NIL "List of all Hunchentoot acceptors that run this server.")
(defvar *radiance-handlers*    NIL "List of Hunchentoot handlers.")
(defvar *radiance-request*     NIL "Current request object.")
(defvar *radiance-request-count* 0 "Counter for the current amount of requests being handled.")
(defvar *radiance-request-total* 0 "Counter for the total amount of requests handled.")
(defvar *radiance-reply*       NIL "Current reply object.")
(defvar *radiance-implements*  (make-hash-table) "Radiance implements table.")
(defvar *radiance-modules*     (make-hash-table) "Map of all loaded modules.")
(defvar *radiance-hooks*       (make-hash-table) "Map of all registered triggers.")
(defvar *radiance-session*     NIL "Current session object, if any,")
(defvar *radiance-api-formats* (make-hash-table) "Map of all API output formats.")

(defvar *random-string-characters* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890123456789" "Default random characters appearing in make-random-string.")
(defvar *default-cookie-expire* (* 60 60 24 356) "Default expiration time in seconds.")
(defconstant *unix-epoch-difference* (encode-universal-time 0 0 0 1 1 1970 0) "Time difference between unix epoch and Lisp time.")
(defvar *uri-matcher* (cl-ppcre:create-scanner "((\\w+\\.)*?)(\\w+(\\.[a-z]+)?)?(:\\d+)?/(.*)"))

(declaim (fixnum *radiance-request-count* *radiance-request-total* *default-cookie-expire* *unix-epoch-difference* *radiance-startup-time*))
(declaim (hash-table *radiance-implements* *radiance-modules* *radiance-hooks* *radiance-api-formats*))
(declaim (string *random-string-characters*))
