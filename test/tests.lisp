(in-package svg.test)

(defvar *markup* "<svg id='container1' class='container square'><a href=\"/test\" target=\"_blank\"><circle class='square'></circle><html:video src='/my-server'/>><</a><title>testing</title></svg>")

(defvar *in-html* (concatenate 'string "<html>" *markup* "</html>"))

(defvar *parsed-markup* (parse-document (make-instance 'svg-document-node :document *markup*)))

(defvar *parsed-embedded* (parse-document (make-instance 'svg-document-node :document *in-html*)))

(define-test parse-markup...
  :parent test-parse
  (of-type 'svg-document-node *parsed-markup*)
  (let ((element (car (get-elements-by-tagname *parsed-markup* "a" *svg-element-class-map*))))
    (of-type 'svg-a element)
    (of-type 'svg.parse::svg-element-class (class-of element)))
  (of-type 'element-node (car (slot-value *parsed-markup* 'child-nodes)))
  (of-type 'svg (get-element-with-attribute *parsed-markup* "id"))
  (of-type 'svg (get-element-with-attribute-value *parsed-markup* "id" "container1"))
  ;; this is svg where attribute values can only be a string. This should be false
  (let ((element (get-element-with-attribute-value *parsed-markup* "class" "container")))
    (true (typep element 'svg)))
  (let ((element (get-elements-with-attribute-value *parsed-markup* "class" "container" "square")))
    (true (typep (car element) 'svg))
    (true (typep (cadr element) 'circle)))
  (let ((element (get-element-with-attribute-values *parsed-markup* "class" "container" "contained")))
    (false (typep element 'svg)))
  (let ((text-nodes (retrieve-text-nodes *parsed-markup*)))
    (is string= ">" (text (car text-nodes)))
    (is string= "<" (text (cadr text-nodes)))
    (of-type 'svg-title (get-next-sibling (parent-node (car text-nodes))))))

(define-test reader...
  :parent test-parse
  (when (readerp)
    (remove-reader))
  (true (set-reader #'read-svg))
  (let* ((document-node (read-from-string "<a href='/some-url'>url</a>"))
	 (child-node (car (slot-value document-node 'child-nodes))))
    (true (slot-exists-p child-node 'href))
    (is string= "/some-url" (slot-value child-node 'href))
    (is string= "url" (text (car (slot-value child-node 'child-nodes))))
    (of-type 'readtable (remove-reader))))

(define-test errors-and-generic-nodes...
  :parent test-parse
  (setf *mode* :strict)
  (fail (parse-document "<custom-node custom-slot='value'>a custom node</custom-node>") 'class-not-found-error)
  (setf *mode* :silent)
  (let* ((document-node (parse-document "<custom-node custom-slot='value'>a custom node</custom-node>"))
	 (child-node (car (slot-value document-node 'child-nodes))))
    (of-type 'generic-node child-node)
    (is string= "a custom node" (text (car (retrieve-text-nodes-with-token document-node "a custom node"))))
    (of-type 'generic-node (get-element-with-attribute document-node "custom-slot"))))
