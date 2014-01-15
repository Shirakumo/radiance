#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-markdown)

(define-interface-method parser:parse (text)
  (with-output-to-string (stream)
    (3bmd:parse-string-and-print-to-stream (string text) stream)))
