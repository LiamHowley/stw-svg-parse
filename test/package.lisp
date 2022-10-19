(defpackage svg.test
  (:use :cl :parachute :xml.parse :svg.parse)
  (:shadow :result)
  (:export :test-parse))

(in-package svg.test)

(define-test test-parse)
