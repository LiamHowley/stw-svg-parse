(in-package svg.parse)

(defmethod print-slot ((object dom-node) (slot svg-direct-slot-definition) (type (eql 'svg-global-event-attribute)) (stream stream))
  (let* ((slot-value (slot-value object (slot-definition-name slot)))
	 (class (class-of slot-value)))
    (dolist (slot-definition (filter-slots-by-type (class-of object) 'standard-direct-slot-definition))
      (let ((slot-name (slot-definition-name slot-definition)))
	(print-slot slot-value definition (slot-definition-type slot-definition) stream)))))


(defmethod print-slot ((object dom-node) (slot svg-direct-slot-definition) (type (eql 'svg-aria-*)) (stream stream))
  (let* ((slot-value (slot-value object (slot-definition-name slot)))
	 (class (class-of slot-value)))
    (dolist (slot-definition (filter-slots-by-type (class-of object) 'standard-direct-slot-definition))
      (let ((slot-name (slot-definition-name slot-definition)))
	(print-slot slot-value definition (slot-definition-type slot-definition) stream)))))
