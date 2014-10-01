#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-sqlite)

;; Add our data path to the lib search function so people
;; can put the dlls or whatnots in there.
(push radiance:*data-path* cffi:*foreign-library-directories*)

;; Load sqlite
(cond
  ((find-package "QL")
   (funcall (find-symbol "QUICKLOAD" "QL") :sqlite))
  ((find-package "ASDF")
   (funcall (find-symbol "LOAD-SYSTEM" "ASDF") :sqlite))
  (T (error "Neither Quicklisp nor ASDF could be found. What is going on?")))
