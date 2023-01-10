(in-package svg.parse)

(defmethod print-slot ((object dom-node) (slot svg-direct-slot-definition) (type (eql 'svg-global-event-attribute)) (stream stream))
  (when (slot-boundp object (slot-definition-name slot))
    (let ((event-class (slot-value object (slot-definition-name slot))))
      (dolist (slot-definition (filter-slots-by-type (class-of event-class) 'standard-direct-slot-definition))
	(let ((slot-name (slot-definition-name slot-definition)))
	  (when (slot-boundp event-class slot-name)
	    (print-slot event-class slot-definition 'string stream)))))))


(defmethod print-slot ((object dom-node) (slot svg-direct-slot-definition) (type (eql 'svg-aria-*)) (stream stream))
  (when (slot-boundp object (slot-definition-name slot))
    (let ((aria-class (slot-value object (slot-definition-name slot))))
      (dolist (slot-definition (filter-slots-by-type (class-of aria-class) 'standard-direct-slot-definition))
	(let ((slot-name (slot-definition-name slot-definition)))
	  (when (slot-boundp aria-class slot-name)
	    (print-slot aria-class slot-definition 'string stream)))))))
