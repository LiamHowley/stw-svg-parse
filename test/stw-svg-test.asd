(defsystem #:stw-svg-test
  :description "Test suite for stw-svg-parse."
  :depends-on ("parachute"
	       "stw-xml-parse"
	       "stw-svg-parse")
  :serial t
  :components ((:file "package")
	       (:file "tests"))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :svg.test)))
