(in-package svg.parse)


(declaim (ftype (function (character symbol) (or function null)) attribute-value-predicate)
	 (inline attribute-value-predicate))

(defun attribute-value-predicate (char type)
  (declare (optimize (speed 3) (safety 0)))
  (case type
    ((cons list array)
     (ecase char
       (#\' (match-character #\space #\'))
       (#\" (match-character #\space #\"))))
    (t
     (case char
       (#\' (match-character #\'))
       (#\" (match-character #\"))
       (t (match-character #\> #\space #\/ #\!))))))


(defmethod read-attribute-value ((slot svg-direct-slot-definition) attribute slot-type)
  (declare (inline match-character))
  (if *embedded*
      ;;behaves like html node
      (let ((char (stw-read-char)))
	(case char
	  (#\=
	   (next)
	   (read-attribute-value slot attribute slot-type))
	  ((#\" #\')
	   (next)
	   (let ((predicate (attribute-value-predicate char slot-type)))
	     (cond ((eq slot-type 'boolean)
		    (let ((value (read-until predicate)))
		      (the boolean (string-equal value attribute))))
		   (t
		    (prog1 
			(let ((value (read-into slot-type predicate)))
			  (if (char= (stw-read-char) char)
			      value
			      (restart-case
				  (multiple-value-error "The attribute ~a does not support multiple values" attribute)
				(use-value (user-supplied)
				  user-supplied)
				(use-first-found-value ()
				  :report "Use first found value and skip the rest"
				  (consume-until (match-character char))
				  value)
				(ignore-attribute ()
				  :report "Ignore all values."
				  (consume-until (match-character char))
				  nil))))
		      (next))))))
	  ((#\newline #\>)
	   nil)
	  ((#\space #\>)
	   (the boolean 
		(cond ((eq slot-type 'boolean)
		       t)
		      (t nil))))
	  (t 
	   (let ((predicate (attribute-value-predicate char slot-type)))
	     (read-into slot-type predicate)))))
      (call-next-method)))


(defmethod map-attribute ((res (eql 'event-*)) attribute length)
  (declare (ignore length))
  (read-until (match-character #\space #\= #\> #\/)))

(defmethod map-attribute ((res (eql 'data-*)) attribute length)
  (declare (ignore length res))
  (read-until (match-character #\space #\= #\> #\/)))

(defmethod map-attribute ((res (eql 'svg-aria-*)) attribute length)
  "svg-aria is so named so as not to confuse with aria-* in html.
Aria in html varies far more widely among element types than in svg. 
As such it is handled differently."
  (declare (ignore length res))
  (read-until (match-character #\space #\= #\> #\/)))


(defmethod assign-value ((class element-node) (slot-name (eql 'svg-event-*)) attribute value)
  (with-slots (svg-event-*) class
    (setf (slot-value svg-event-* (find-symbol attribute 'svg.parse)) value)))

(defmethod assign-value ((class element-node) (slot-name (eql 'svg-aria-*)) attribute value)
  (with-slots (svg-aria-*) class
    (setf (slot-value svg-aria-* (find-symbol attribute 'svg.parse)) value)))

(defmethod assign-value ((class element-node) (slot-name (eql 'data-*)) attribute value)
  (push (cons attribute value) (slot-value class 'data-*)))
