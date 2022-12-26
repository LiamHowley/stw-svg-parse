(defpackage svg.test
  (:use :cl :parachute :xml.parse :svg.parse)
  (:import-from
   :html.parse
   :html)
  (:export :test-parse))

(in-package svg.test)

(define-test test-parse)
