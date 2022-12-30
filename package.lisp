(in-package :cl-user)

(defpackage svg.parse
  (:use :cl :xml.parse)

  (:import-from
   :contextl
   :singleton-class)

  (:import-from
   :stw.util
   :stw-read-char
   :next
   :ensure-list
   :parse-stream
   :match-string
   :match-character
   :consume-until
   :read-until
   :read-and-decode)

  (:import-from
   :closer-mop
   :slot-definition-name
   :slot-definition-type
   :effective-slot-definition-class
   :direct-slot-definition-class
   :compute-effective-slot-definition
   :validate-superclass)

  (:export

   ;; specials
   :*svg-element-class-map*
   :*preserve-whitespace*
   :*end-script*
   :*end-style*

   ;; meta
   :element-class
   :svg-direct-slot-definition

   ;; model
   :define-svg-node
   :document-node
   :dom-node
   :standard-element-node
   :element-node
   :branch-node
   :leaf-node
   :content-node
   :text-node
   :whitespace-node
   :svg

   :sgml-node
   :!--

   ;; functions
   :parse-document
   :read-element
   :read-element-name
   :read-element-attributes
   :read-attribute
   :read-attribute-value
   :read-content
   :read-into
   :read-subelements
   :bind-child-node
   :attribute-value-predicate
   :assign-value
   :parse-value
   :get-element-name
   :map-attribute-to-slot
   :map-attribute
   :serialize
   :write-to-file)

  (:export
   :find-ancestor-node
   :clone-node
   :walk-tree
   :retrieve-text-nodes
   :retrieve-text-nodes-with-token
   :retrieve-text-nodes-with-tokens
   :retrieve-text-nodes-with-all-tokens
   :get-elements-by-tagname
   :get-element-with-attribute
   :get-element-with-attributes
   :get-element-with-attribute-value
   :get-element-with-attribute-values
   :get-elements-with-attribute
   :get-elements-with-attributes
   :get-elements-with-attribute-value
   :get-elements-with-attribute-values
   :get-next-sibling
   :get-previous-sibling
   :query-select
   :query-select-all
   :remove-node
   :add-node
   :insert-before
   :insert-after
   :first-of-type
   :last-of-type)

  (:export
   :remove-reader
   :set-reader
   :read-svg))
