(in-package #:org.shirakumo.radiance.test)

(define-test cache
  :parent interfaces
  (let ((cached-value (make-array 1 :element-type '(unsigned-byte 8)
                                    :initial-contents '(42)))
        (uncached-value (make-array 1 :element-type '(unsigned-byte 8)
                                      :initial-contents '(43))))
    (cache:renew 'cache-name-1)
    (is eq nil (cache:get 'cache-name-1))
    (cache:with-cache (cache-name-1) nil
      cached-value)
    (cache:with-cache (cache-name-1) nil
      uncached-value)
    (is equalp cached-value (cache:get 'cache-name-1))
    (cache:renew 'cache-name-1)
    (is eq nil (cache:get 'cache-name-1))))
