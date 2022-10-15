(in-package svg.parse)

(defmethod bind-child-node ((parent-node branch-node) (child-node svg))
  ;; as there are overlapping/duplicate element names/classes between svg and html
  ;; we need to specify the correct hash-table for reading.
  (let ((*element-class-map* *svg-element-class-map*))
    (call-next-method)))
