#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defmodule flash-dispatch (dispatcher)
  "Simple static lookup-table dispatcher"
  (:name "Flash Dispatcher" :author "Nicolas Hafner" :version "0.0.1" :license "Artistic" :url "http://tymoon.eu")
  (table :initform (make-hash-table) :accessor table :allocation :class))

(implement 'dispatcher (get-module 'flash-dispatch))

(defmethod dispatch ((dispatch flash-dispatch) request &key)
  "Dispatches a request to a module based on a subdomain."
  (trigger (gethash (first (slot-value request 'subdomains)) (table dispatch))))

(defmethod register ((dispatch flash-dispatch) trigger &key subdomain)
  "Registers a subdomain for a module. If the subdomain is NIL, all unhandled requests are dispatched to the module."
  (setf (gethash subdomain (table dispatch)) trigger))
