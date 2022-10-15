(in-package svg.parse)

(defclass svg-element-class (element-class)
  ((status :initform :active
	   :initarg :status
	   :reader status)))

(defclass svg-direct-slot-definition (xml-direct-slot-definition)
  ((animatable :initarg :animateable :initform t :type boolean :reader animatable))
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
