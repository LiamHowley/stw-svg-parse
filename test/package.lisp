(defpackage svg.test
  (:use :cl :parachute :xml.parse :svg.parse)
  (:export :test-parse))

(in-package svg.test)

(define-test test-parse)
