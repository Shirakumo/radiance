#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

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
  (encrypt (ironclad:ascii-string-to-byte-array text) key :mode mode :IV IV))

(defmethod encrypt ((text vector) key &key (mode 'ironclad:ecb) (IV (ironclad:make-random-salt)))
  (let ((cipher (get-cipher key :mode mode :IV IV)))
    (ironclad:encrypt-in-place cipher text)
    (values (ironclad:octets-to-integer text)
            key mode IV)))

(defmethod decrypt ((text string) key &key (mode 'ironclad:ecb) IV)
  (decrypt (parse-integer text) key :mode mode :IV IV))

(defmethod decrypt ((text integer) key &key (mode 'ironclad:ecb) IV)
  (decrypt (ironclad:integer-to-octets text) key :mode mode :IV IV))

(defmethod decrypt ((text vector) key &key (mode 'ironclad:ecb) IV)
  (let ((cipher (get-cipher key :mode mode :IV IV)))
    (ironclad:decrypt-in-place cipher text)
    (values (coerce (mapcar #'code-char (coerce text 'list)) 'string)
            key mode IV)))


(defgeneric make-salt (salt)
  (:documentation "Create a salt."))

(defgeneric pbkdf2-hash (password salt &key digest iterations)
  (:documentation "Compute a PBKDF2 hash for the provided password and salt."))

(defgeneric simple-hash (password salt &key digest iterations)
  (:documentation "Compute a simple hash for the provided password and salt using the given digest."))

(defmethod make-salt ((salt T)) (ironclad:make-random-salt))
(defmethod make-salt ((salt integer)) (ironclad:make-random-salt salt))
(defmethod make-salt ((salt string)) (ironclad:ascii-string-to-byte-array salt))
(defmethod make-salt ((salt vector)) salt)

(defmethod pbkdf2-hash ((password string) salt &key (digest 'ironclad:sha512) (iterations 1000))
  (setf salt (make-salt salt))
  (values (ironclad:byte-array-to-hex-string
           (ironclad:pbkdf2-hash-password (ironclad:ascii-string-to-byte-array password)
                                          :salt salt :digest digest :iterations iterations))
          (ironclad:byte-array-to-hex-string salt)
          digest iterations))

(defmethod simple-hash  ((password string) salt &key (digest 'ironclad:sha512) (iterations 1000))
  (setf salt (make-salt salt))
  (values (ironclad:byte-array-to-hex-string
           (let ((hash (ironclad:make-digest digest)))
             (ironclad:update-digest hash salt)
             (ironclad:update-digest hash (ironclad:ascii-string-to-byte-array password))
             (dotimes (x iterations)
               (ironclad:update-digest hash (ironclad:produce-digest hash)))
             (ironclad:produce-digest hash)))
          (ironclad:byte-array-to-hex-string salt)
          digest iterations))
