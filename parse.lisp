(in-package svg.parse)

(defmethod parse-document ((document svg-document-node) &key (parser #'read-element) preserve-whitespace)
  (let ((*end-script* (read-until (match-string "</script>" nil)))
	(*end-style* (read-until (match-string "</style>" nil)))
	(*element-class-map* *svg-element-class-map*))
    (call-next-method)))

(defmethod initialize-instance :around ((child-node svg) &key)
  "As there are overlapping/duplicate element names/classes between svg and html
   we need to specify the correct hash-table for reading."
  (let ((*element-class-map* *svg-element-class-map*))
    (call-next-method)))


(defvar *embedded* nil "SVG inherits html rules and behaviours when embedded in a html doc, 
otherwise it is bound by XML rules.")


(defmethod bind-node ((parent-node dom-node) (child-node svg))
  (let ((*embedded* t))
    (call-next-method)))


(defmethod read-attribute-value ((slot html-direct-slot-definition) attribute slot-type)
  (declare (inline match-character))
  (if *embedded*
      (let* ((char (stw-read-char))
	     (reader (read-and-decode (match-character char))))
	(case char
	  (:eof nil)
	  (#\=
	   (next)
	   (read-attribute-value slot attribute slot-type))
	  ((#\" #\')
	   (next)
	   (funcall reader))
	  ((#\newline #\space #\>)
	   nil)
	  (t 
	   (funcall
	    (read-and-decode #'(lambda (test-char)
				 (when (or (char= test-char #\space)
					   (char= test-char #\>))
				   test-char)))))))
      (call-next-method)))


(defmethod map-attribute ((res (eql 'event-*)) attribute length)
  (declare (ignore length))
  (prepare-slot node result)
  (funcall *next-attribute*))

(defmethod map-attribute ((res (eql 'data-*)) attribute length)
  (declare (ignore length res))
  (funcall *next-attribute*))

(defmethod map-attribute ((res (eql 'svg-aria-*)) attribute length)
  "svg-aria is so named so as not to confuse with aria-* in html.
Aria in html varies far more widely among element types than in svg. 
As such it is handled differently."
  (declare (ignore length res))
  (funcall *next-attribute*))



(defmethod prepare-slot
    ((class element-node) (slot (eql 'svg-event-*)))
  (unless (slot-boundp class 'event-*)
    (setf (slot-value class 'event-*) (make-instance 'svg-global-event-attribute))))


(defmethod assign-value
    ((class element-node) (slot svg-direct-slot-definition) slot-name attribute value)
  (unless (assign-slot-value class slot-name attribute value)
    (call-next-method)))

(defmethod assign-slot-value ((class element-node) (slot (eql 'svg-event-*)) attribute value)
  (declare (ignore slot))
  (with-slots (svg-event-*) class
    (setf (slot-value svg-event-* (find-symbol attribute 'svg.parse)) value)))

(defmethod assign-slot-value ((class element-node) (slot (eql 'svg-aria-*)) attribute value)
  (declare (ignore slot))
  (with-slots (svg-aria-*) class
    (setf (slot-value svg-aria-* (find-symbol attribute 'svg.parse)) value)))

(defmethod assign-slot-value ((class element-node) (slot (eql 'data-*)) attribute value)
  (push (cons attribute value) (slot-value class 'data-*)))
