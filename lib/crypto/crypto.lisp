#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-lib-crypto)

(defun byte-array-to-ascii-string (array)
  (coerce (mapcar #'code-char (coerce array 'list)) 'string))

(defgeneric to-base64 (sequence)
  (:documentation "Turns a sequence into a base64-encoded string using UTF-8 encoding."))

(defgeneric from-base64 (sequence)
  (:documentation "Turns a base64-encoded sequence into an UTF-8 string."))

(defmethod to-base64 ((integer integer))
  (base64:integer-to-base64-string integer))

(defmethod to-base64 ((array array))
  (base64:usb8-array-to-base64-string array))

(defmethod to-base64 ((string string))
  (to-base64
   (flexi-streams:string-to-octets string :external-format :utf-8)))

(defmethod from-base64 ((string string))
  (flexi-streams:octets-to-string
   (base64:base64-string-to-usb8-array string)
   :external-format :utf-8))

(defmethod from-base64 ((vector vector))
  (from-base64
   (byte-array-to-ascii-string vector)))

(defgeneric get-cipher (key &key mode IV)
  (:documentation "Return the corresponding cipher."))

(defgeneric encrypt (text key &key mode IV)
  (:documentation "Encrypt the text with the provided key, using the specified AES mode."))

(defgeneric decrypt (text key &key mode IV)
  (:documentation "Decrypt the text with the provided key, using the specified AES mode."))

(defmethod get-cipher ((key string) &key mode IV)
  (get-cipher (ironclad:ascii-string-to-byte-array key) :mode mode :IV IV))

(defmethod get-cipher ((key vector) &key mode IV)
  (ironclad:make-cipher 'ironclad:aes :key key :mode mode :initialization-vector IV))

(defmethod encrypt ((text string) key &key (mode 'ironclad:ecb) (IV (ironclad:make-random-salt)))
  (encrypt (flexi-streams:string-to-octets text :external-format :utf-8) key :mode mode :IV IV))

(defmethod encrypt ((text vector) key &key (mode 'ironclad:ecb) (IV (ironclad:make-random-salt)))
  (let ((text (ironclad:ascii-string-to-byte-array (base64:usb8-array-to-base64-string text)))
        (cipher (get-cipher key :mode mode :IV IV)))
    (ironclad:encrypt-in-place cipher text)
    (values (to-base64 text)
            key mode IV)))

(defmethod decrypt ((text string) key &key (mode 'ironclad:ecb) IV)
  (decrypt (base64:base64-string-to-usb8-array text) key :mode mode :IV IV))

(defmethod decrypt ((text integer) key &key (mode 'ironclad:ecb) IV)
  (decrypt (ironclad:integer-to-octets text) key :mode mode :IV IV))

(defmethod decrypt ((text vector) key &key (mode 'ironclad:ecb) IV)
  (let ((cipher (get-cipher key :mode mode :IV IV)))
    (ironclad:decrypt-in-place cipher text)
    (values (from-base64 text) key mode IV)))

(defgeneric make-salt (salt)
  (:documentation "Create a salt."))

(defmethod make-salt ((salt T)) (ironclad:make-random-salt))
(defmethod make-salt ((salt integer)) (ironclad:make-random-salt salt))
(defmethod make-salt ((salt string)) (ironclad:ascii-string-to-byte-array salt))
(defmethod make-salt ((salt vector)) salt)

(defun pbkdf2-key (password salt &key (digest 'ironclad:sha512) (iterations 1000))
  (setf salt (make-salt salt))
  (values (ironclad:pbkdf2-hash-password (ironclad:ascii-string-to-byte-array (to-base64 password))
                                         :salt salt :digest digest :iterations iterations)
          (byte-array-to-ascii-string salt)
          digest iterations))

(defun pbkdf2-hash (password salt &key (digest 'ironclad:sha512) (iterations 1000))
  (setf salt (make-salt salt))
  (values (ironclad:byte-array-to-hex-string
           (ironclad:pbkdf2-hash-password (ironclad:ascii-string-to-byte-array (to-base64 password))
                                          :salt salt :digest digest :iterations iterations))
          (byte-array-to-ascii-string salt)
          digest iterations))

(defun simple-hash  (password salt &key (digest 'ironclad:sha512) (iterations 1000))
  (setf salt (make-salt salt))
  (values (ironclad:byte-array-to-hex-string
           (let ((hash (ironclad:make-digest digest)))
             (ironclad:update-digest hash salt)
             (ironclad:update-digest hash (ironclad:ascii-string-to-byte-array (to-base64 password)))
             (dotimes (x iterations)
               (ironclad:update-digest hash (ironclad:produce-digest hash)))
             (ironclad:produce-digest hash)))
          (byte-array-to-ascii-string salt)
          digest iterations))
