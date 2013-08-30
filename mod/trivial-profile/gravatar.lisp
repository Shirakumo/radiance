#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-profile)

(defun hash (email)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence 
    :md5 (ironclad:ascii-string-to-byte-array (string-trim " " (string-downcase email))))))

(defun encode (s arg colonp atp &rest args)
  (declare (ignore colonp atp args))
  (format s "~a=~a" (car arg) (drakma:url-encode (cdr arg) :utf-8)))

(defun gravatar-image (email &key size default force-default-p rating)
  (format nil "https://secure.gravatar.com/avatar/~a~@[?~{~/radiance-mod-trivial-profile::encode/~^&~}~]"
   (hash email)
   (append 
    (if size            `(("s" . ,(format nil "~d" size))))
    (if force-default-p `(("f" . "y")))
    (if rating          `(("r" . ,(string-downcase rating))))
    (if default 
        (typecase default
          (keyword      `(("d" . ,(string-downcase default))))
          (string       `(("d" . ,default))))))))
