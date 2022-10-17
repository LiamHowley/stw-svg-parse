(defsystem #:stw-svg-parse
  :author "Liam Howley <liam.howley@thespanningtreeweb.ie>"
  :license "MIT"
  :depends-on ("contextl"
	       "closer-mop"
	       "stw-xml-parse"
	       "stw-utils")
  :description "A SVG to DOM style parser; parsing elements and attributes into CLOS objects."
  :serial t
  :components ((:file "package")
	       (:file "meta")
	       (:file "model")
	       (:file "parse")
	       (:file "read")
	       (:file "print"))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "docs/README.org")))
