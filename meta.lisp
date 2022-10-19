(in-package svg.parse)

(defclass svg-element-class (element-class)
  ((status :initform :active
	   :initarg :status
	   :reader status)))

(defclass svg-direct-slot-definition (xml-direct-slot-definition)
  ((animatable :initarg :animatable :initform t :type boolean :reader animatable))
  (:documentation "default class representing svg attribute slots"))

(defclass svg-effective-slot-definition (xml-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class
    ((class svg-element-class) &key &allow-other-keys)
  (find-class 'svg-direct-slot-definition))

(defmethod effective-slot-definition-class
    ((class svg-element-class) &key &allow-other-keys)
  (call-next-method))

(defmethod compute-effective-slot-definition
    ((class svg-element-class) name direct-slot-definitions)
  (call-next-method))

(defmethod validate-superclass
    ((class svg-element-class)
     (superclass element-class))
  t)

(defmethod validate-superclass
    ((superclass element-class)
     (class svg-element-class))
  t)

(defvar *svg-element-class-map*
  (let ((table (make-hash-table :test #'equal)))
    (maphash #'(lambda (element class)
		 (setf (gethash element table) class))
	     *element-class-map*)
    table)
  "Copying the elements from XML.PARSE:*ELEMENT-CLASS-MAP* 
as they may well be called upon during parsing.")

(defmethod shared-initialize :around ((class svg-element-class) slot-names &key)
  (declare (ignore slot-names))
  ;; as there are overlapping/duplicate element names/classes between svg and html
  ;; we need to specify the correct hash-table for writing.
  (let ((*element-class-map* *svg-element-class-map*))
    (call-next-method)))
