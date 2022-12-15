(in-package svg.parse)

(defvar *end-script*)

(defvar *end-style*)

(defclass svg-document-node (xml-document-node)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass animation-element ()
    ()
    (:metaclass singleton-class))

  (defclass basic-shape ()
    ()
    (:metaclass singleton-class))

  (defclass container-element ()
    ()
    (:metaclass singleton-class))

  (defclass descriptive-element ()
    ()
    (:metaclass singleton-class))

  (defclass filter-primitive-element ()
    ()
    (:metaclass singleton-class))

  (defclass font-element ()
    ()
    (:metaclass singleton-class))

  (defclass gradient-element ()
    ()
    (:metaclass singleton-class))

  (defclass graphics-element ()
    ()
    (:metaclass singleton-class))

  (defclass graphics-referencing-element ()
    ()
    (:metaclass singleton-class))

  (defclass light-source-element ()
    ()
    (:metaclass singleton-class))

  (defclass never-rendered-element ()
    ()
    (:metaclass singleton-class))

  (defclass paint-server-element ()
    ()
    (:metaclass singleton-class))

  (defclass renderable-element ()
    ()
    (:metaclass singleton-class))

  (defclass shape-element ()
    ()
    (:metaclass singleton-class))

  (defclass structural-element ()
    ()
    (:metaclass singleton-class))

  (defclass text-content-element ()
    ()
    (:metaclass singleton-class))

  (defclass text-content-child-element ()
    ()
    (:metaclass singleton-class))

  (defclass uncategorized-element ()
    ()
    (:metaclass singleton-class))


  (defclass-with-initargs svg-global-event-attribute ()
    (oncancel oncanplay oncanplaythrough onchange onclick onclose oncuechange ondblclick ondrag ondragend ondragenter ondragleave ondragover ondragstart ondrop ondurationchange onemptied onended onerror onfocus oninput oninvalid onkeydown onkeypress onkeyup onload onloadeddata onloadedmetadata onloadstart onmousedown onmouseenter onmouseleave onmousemove onmouseout onmouseover onmouseup onmousewheel onpause onplay onplaying onprogress onratechange onreset onresize onscroll onseeked onseeking onselect onshow onstalled onsubmit onsuspend ontimeupdate ontoggle onvolumechange onwaiting))


  ;; Note: aria here is handled differently from aria in STW-HTML-PARSE, as other than a few assigned attributes,
  ;; the list of aria attributes for svg elements is constant. This is not the same for html, where aria attributes
  ;; are highly variable across the element tree.

  (defclass-with-initargs svg-aria-* ()
    (aria-activedescendant aria-atomic aria-autocomplete aria-busy aria-checked aria-colcount aria-colindex aria-colspan aria-controls aria-current aria-describedby aria-details aria-disabled aria-dropeffect aria-errormessage aria-expanded aria-flowto aria-grabbed aria-haspopup aria-hidden aria-invalid aria-keyshortcuts aria-label aria-labelledby aria-live aria-modal aria-multiline aria-multiselectable aria-orientation aria-owns aria-placeholder aria-posinset aria-pressed aria-readonly aria-relevant aria-required aria-roledescription aria-rowcount aria-rowindex aria-rowspan aria-selected aria-setsize aria-sort aria-valuemax aria-valuemin aria-valuenow aria-valuetext aria-level role))

  (defvar *svg-presentation-attributes*
    `((svg-fill :attribute "fill")
      alignment-baseline baseline-shift clip-path clip-rule color color-interpolation color-interpolation-filters color-rendering cursor direction display dominant-baseline fill-opacity fill-rule filter flood-color flood-opacity font-family font-size font-size-adjust font-stretch font-style font-variant font-weight glyph-orientation-horizontal glyph-orientation-vertical image-rendering letter-spacing lighting-color marker-end marker-mid marker-start mask opacity overflow paint-order pointer-events shape-rendering stop-color stop-opacity stroke stroke-dasharray stroke-dashoffset stroke-linecap stroke-linejoin stroke-miterlimit stroke-opacity stroke-width text-anchor text-decoration text-overflow text-rendering unicode-bidi vector-effect visibility white-space word-spacing writing-mode))

  (defvar *svg-global-attributes*
    '((svg-space :attribute "xml:space" :initarg :space :reader svg-space)
      (lang :attribute "xml:lang" :animatable nil)
      (base :attribute "xml:base" :initarg :base :reader base)
      (data-* :initarg :data-* :type multiple-attributes)
      (event-* :initarg :event-* :type svg-global-event-attribute)
      (svg-class :attribute "class" :initarg :class :type cons :accessor svg-class)
      (svg-style :attribute "style")
      id tabindex))

  (defvar *animation-nodes* `(animate animate-motion animate-transform svg-set))

  (defvar *svg-animation-attributes*
    '((repeat-dur :attribute "repeatDur")
      (repeat-count :attribute "repeatCount")
      (calc-mode :attribute "calcMode")
      (key-times :attribute "keyTimes")
      (key-splines :attribute "keySplines")
      (animate-min :attribute "min" :initarg :min)
      (animate-max :attribute "max" :initarg :max)
      (required-extensions :attribute "requiredExtensions")
      (system-language :attribute "systemLanguage")
      (svg-restart :attribute "restart")
      (svg-values :attribute "values")
      href begin by dur end from to additive accumulate onbegin onend onrepeat)))


(defmacro define-svg-node (name supers slots &rest rest)
  "Wrapper on DEFINE-ELEMENT-NODE macro. "
  ;; add global slots
  (flet ((include-slots (slots%)
	   (loop
	     for slot in slots%
	     do (pushnew slot slots :test #'eq))))
    (include-slots *svg-global-attributes*)
    (include-slots *svg-presentation-attributes*)
    (when (member name *animation-nodes* :test #'eq)
      (unless (eq name 'svg-set)
	(include-slots *svg-animation-attributes*))))
  `(define-element-node ,name ,supers
     ,(loop
	for slot in slots
	do (setf slot (ensure-list slot))
	do (when (member name *animation-nodes* :test #'eq)

	     ;; set animatable to nil for animation attributes
	     ;; and add common attributes to relevant slots.
	     (pushnew nil (cdr slot))
	     (pushnew :animatable (cdr slot)))
	collect slot)
     (:metaclass svg-element-class)
     ,@rest))


(define-svg-node svg-a (renderable-element container-element)
  ((a-type :attribute "type")
   (svg-aria-* :type svg-aria-*)
   (download :animatable nil)
   (hreflang :animatable nil)
   (ping :animatable nil)
   (referrerpolicy :animatable nil)
   (rel :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   (system-language :attribute "systemLanguage" :animatable nil)
   (xlink-href :attribute "xlink:href" :animatable nil)
   (xlink-title :attribute "xlink:title" :animatable nil)
   href target)
  (:element . "a"))

(define-svg-node animate (animation-element)
  ((attribute-name :attribute "attributeName")))

(define-svg-node animate-motion (animation-element)
  ((key-points :attribute "keyPoints")
   origin path rotate)
  (:element . "animateMotion"))


(define-svg-node animate-transform (animation-element)
  ((animate-transform-type :attribute "type")
   (attribute-name :attribute "attributeName"))
  (:element . "animateTransform"))

(define-svg-node circle (shape-element renderable-element graphics-element basic-shape)
  ((svg-aria-* :type svg-aria-*)
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   (system-language :attribute "systemLanguage" :animatable nil)
   (path-length :attribute "pathLength")
   cx cy r))

(define-svg-node clip-path (uncategorized-element never-rendered-element)
  ((clip-path-units :attribute "clipPathUnits")
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil))
  (:element . "clipPath"))

(define-svg-node defs (structural-element never-rendered-element container-element)
  ())

(define-svg-node desc (descriptive-element)
  ())

(define-svg-node discard (animation-element)
  ((svg-aria-* :type svg-aria-*)
   (begin :animatable nil)
   (href :animatable nil)
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)))

(define-svg-node ellipse (shape-element renderable-element graphics-element basic-shape)
  ((svg-aria-* :type svg-aria-*)
   (path-length :attribute "pathLength")
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)))

(define-svg-node fe-blend (filter-primitive-element)
  (width height in in2 mode result x y)
  (:element . "feBlend"))

(define-svg-node fe-color-matrix (filter-primitive-element)
  ((fe-color-matrix-type :attribute "type" :initarg :type)
   (svg-values :attribute "values")
   width height fe result x y)
  (:element . "feColorMatrix"))

(define-svg-node fe-component-transfer (filter-primitive-element)
  (width height fe result x y)
  (:element . "feComponentTransfer"))

(define-svg-node fe-composite (filter-primitive-element)
  (width height fe fe2 k1 k2 k3 k4 operator result x y)
  (:element . "feComposite"))

(define-svg-node fe-convolve-matrix (filter-primitive-element)
  ((edge-mode :attribute "edgeMode")
   (kernel-matrix :attribute "kernelMatrix")
   (kernel-unit-length :attribute "kernelUnitLength")
   (preserve-alpha :attribute "preserveAlpha")
   (target-x :attribute "targetX")
   (target-y :attribute "targetY")
   width height bias divisor fe order result x y)
  (:element . "feConvolveMatrix"))

(define-svg-node fe-diffuse-lighting (filter-primitive-element)
  ((diffuse-constant :attribute "diffuseConstant")
   (kernel-unit-length :attribute "kernelUnitLength")
   (surface-scale :attribute "surfaceScale")
   width height fe result x y)
  (:element . "feDiffuseLighting"))

(define-svg-node fe-displacement-map (filter-primitive-element)
  ((x-channel-selector :attribute "xChannelSelector")
   (y-channel-selector :attribute "yChannelSelector")
   width height fe fe2 result scale x y)
  (:element . "feDisplacementMap"))

(define-svg-node fe-distant-light (light-source-element)
  (azimuth elevation)
  (:element . "feDistantLight"))

(define-svg-node fe-drop-shadow (filter-primitive-element)
  ((std-deviation :attribute "stdDeviation")
   dx dy width height fe result x y)
  (:element . "feDropShadow"))

(define-svg-node fe-flood (filter-primitive-element)
  (width height result x y)
  (:element . "feFlood"))

(define-svg-node fe-func-a (filter-primitive-element)
  ((table-values :attribute "tableValues")
   (func-type :attribute "type" :initarg :type)
   amplitude exponent intercept offset slope)
  (:element . "feFuncA"))

(define-svg-node fe-func-b (filter-primitive-element)
  ((table-values :attribute "tableValues")
   (func-type :attribute "type" :initarg :type)
   amplitude exponent intercept offset slope)
  (:element . "feFuncB"))

(define-svg-node fe-func-g (filter-primitive-element)
  ((table-values :attribute "tableValues")
   (func-type :attribute "type" :initarg :type)
   amplitude exponent intercept offset slope)
  (:element . "feFuncG"))

(define-svg-node fe-func-r (filter-primitive-element)
  ((table-values :attribute "tableValues")
   (func-type :attribute "type" :initarg :type)
   amplitude exponent intercept offset slope)
  (:element . "feFuncR"))

(define-svg-node fe-gaussian-blur (filter-primitive-element)
  ((edge-mode :attribute "edgeMode")
   (std-deviation :attribute "stdDeviation")
   width height fe result x y)
  (:element . "feGaussianBlur"))

(define-svg-node fe-image (filter-primitive-element)
  ((crossorigin :animatable nil)
   (preserve-aspect-ratio :attribute "preserveAspectRatio")
   (xlink-href :attribute "xlink:href")
   (xlink-title :attribute "xlink:title" :animatable nil)
   (svg-image :attribute "image")
   height href result x y)
  (:element . "feImage"))

(define-svg-node fe-merge (filter-primitive-element)
  (width height result x y)
  (:element . "feMerge"))

(define-svg-node fe-merge-node (filter-primitive-element)
  (fe)
  (:element . "feMergeNode"))

(define-svg-node fe-morphology (filter-primitive-element)
  (width height fe operator radius result x y)
  (:element . "feMorphology"))

(define-svg-node fe-offset (filter-primitive-element)
  (dx dy width height fe result x y)
  (:element . "feOffset"))

(define-svg-node fe-point-light (light-source-element)
  (x y z)
  (:element . "fePointLight"))

(define-svg-node fe-specular-lighting (filter-primitive-element)
  ((specular-constant :attribute "specularConstant")
   (specular-exponent :attribute "specularExponent")
   (surface-scale :attribute "surfaceScale")
   width height fe result x y)
  (:element . "feSpecularLighting"))

(define-svg-node fe-spot-light (light-source-element)
  ((limiting-cone-angle :attribute "limitingConeAngle")
   (points-at-x :attribute "pointsAtX")
   (points-at-y :attribute "pointsAtY")
   (points-at-z :attribute "pointsAtZ")
   (specular-exponent :attribute "specularExponent")
   x y z)
  (:element . "feSpotLight"))

(define-svg-node fe-tile (filter-primitive-element)
  (width height fe result x y)
  (:element . "feTile"))

(define-svg-node fe-turbulence (filter-primitive-element)
  ((base-frequency :attribute "baseFrequency")
   (num-octaves :attribute "numOctaves")
   (stitch-tiles :attribute "stitchTiles")
   (func-type :attribute "type" :initarg :type)
   width height result seed x y)
  (:element . "feTurbulence"))

(define-svg-node filter (uncategorized-element)
  ((filter-units :attribute "filterUnits")
   (primitive-units :attribute "primitiveUnits")
   width height x y))

(define-svg-node foreign-object (uncategorized-element renderable-element)
  ((svg-aria-* :type svg-aria-*)
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil))
  (:element . "foreignObject"))

(define-svg-node g (structural-element renderable-element container-element)
  ((svg-aria-* :type svg-aria-*)
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)))

(define-svg-node hatch (paint-server-element never-rendered-element)
  ())

(define-svg-node hatchpath (uncategorized-element)
  ())

(define-svg-node svg-image (renderable-element graphics-element)
  ((svg-aria-* :type svg-aria-*)
   (system-language :attribute "systemLanguage" :animatable nil)
   (preserve-aspect-ratio :attribute "preserveAspectRatio")
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   (xlink-href :attribute "xlink:href" :animatable nil)
   (xlink-title :attribute "xlink:title" :animatable nil)
   href crossorigin)
  (:element . "image"))

(define-svg-node line (shape-element renderable-element graphics-element basic-shape)
  ((svg-aria-* :type svg-aria-*)
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   (path-length :attribute "pathLength")
   x1 y1 x2 y2))

(define-svg-node linear-gradient (paint-server-element never-rendered-element gradient-element)
  ((gradient-transform :attribute "gradientTransform")
   (gradient-units :attribute "gradientUnits")
   (spread-method :attribute "spreadMethod")
   (xlink-href :attribute "xlink:href" :animatable nil)
   (xlink-title :attribute "xlink:title" :animatable nil)
   href x1 y1 x2 y2)
  (:element . "linearGradient"))

(define-svg-node marker (never-rendered-element container-element)
  ((marker-height :attribute "markerHeight")
   (marker-units :attribute "markerUnits")
   (marker-width :attribute "markerWidth")
   (preserve-aspect-ratio :attribute "preserveAspectRatio")
   (ref-x :attribute "refX")   
   (ref-y :attribute "refY")   
   (view-box :attribute "viewBox")
   orient))

(define-svg-node mask (never-rendered-element container-element)
  ((mask-content-units :attribute "maskContentUnits")
   (mask-units :attribute "maskUnits")
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   width height x y))

(define-svg-node metadata (never-rendered-element descriptive-element)
  ())

(define-svg-node mpath (animation-element)
  ((href :animatable nil)
   (path-length :attribute "pathLength")))

(define-svg-node path (shape-element renderable-element graphics-element)
  ((svg-aria-* :initarg :svg-aria-* :type svg-aria-* :reader svg-aria-*)
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)))

(define-svg-node pattern (paint-server-element never-rendered-element container-element)
  ((pattern-content-units :attribute "patternContentUnits")
   (pattern-transform :attribute "patternTransform")
   (pattern-units :attribute "patternUnits")
   (preserve-aspect-ratio :attribute "preserveAspectRatio")
   (view-box :attribute "viewBox")
   (xlink-href :attribute "xlink:href" :animatable nil)
   (xlink-title :attribute "xlink:title" :animatable nil)
   width height href x y))

(define-svg-node polygon (shape-element renderable-element graphics-element basic-shape)
  ((svg-aria-* :type svg-aria-*)
   (path-length :attribute "pathLength")
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   points))

(define-svg-node polyline (shape-element renderable-element graphics-element basic-shape)
  ((svg-aria-* :type svg-aria-*)
   (path-length :attribute "pathLength")
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   points))

(define-svg-node radial-gradient (paint-server-element never-rendered-element gradient-element)
  ((gradient-transform :attribute "gradientTransform")
   (gradient-units :attribute "gradientUnits")
   (spread-method :attribute "spreadMethod")
   (xlink-href :attribute "xlink:href" :animatable nil)
   (xlink-title :attribute "xlink:title" :animatable nil)
   cx cy fr fx fy href r)
  (:element . "radialGradient"))

(define-svg-node rect (shape-element renderable-element graphics-element basic-shape)
  ((svg-aria-* :type svg-aria-*)
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   (path-length :attribute "pathLength")
   width height))

(define-svg-node svg-script (uncategorized-element never-rendered-element content-node)
  ((href :animatable nil)
   (script-type :attribute "type" :initarg :type)
   (xlink-href :attribute "xlink:href" :animatable nil)
   (xlink-title :attribute "xlink:title" :animatable nil)
   crossorigin)
  (:element . "script"))

(defmethod initialize-instance :before ((class svg-script) &key)
  (setf (slot-value class 'closing-tag) *end-script*))

(define-svg-node svg-set (animation-element)
  ((attribute-name :attribute "attributeName")
   (set-min :attribute "min" :initarg :min)
   (set-max :attribute "max" :initarg :max)
   (repeat-count :attribute "repeatCount")
   (repeat-dur :attribute "repeatDur")
   (required-extensions :attribute "requiredExtensions")
   (system-language :attribute "systemLanguage" :animatable nil)
   (svg-restart :attribute "restart")
   dur begin end from href onbegin onend onrepeat))

(define-svg-node solidcolor (paint-server-element)
  ())

(define-svg-node stop (gradient-element)
  (offset))

(define-svg-node svg-style (uncategorized-element never-rendered-element content-node)
  ((media :animatable nil)
   (style-type :attribute "type" :initarg :type)
   (svg-title :animatable nil :attribute "title"))
  (:element . "style"))

(defmethod initialize-instance :before ((class svg-style) &key)
  (setf (slot-value class 'closing-tag) *end-style*))


(define-svg-node svg (structural-element renderable-element container-element)
  ((svg-aria-* :type svg-aria-*)
   (base-profile :attribute "baseProfile" :animatable nil :status :deprecated)
   (content-script-type :attribute "contentScriptType" :animatable nil :status :deprecated)
   (content-style-type :attribute "contentStyleType" :animatable nil :status :deprecated)
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   (system-language :attribute "systemLanguage" :animatable nil)
   (playbackorder :animatable nil)
   (preserve-aspect-ratio :attribute "preserveAspectRatio")
   (onabort :animatable nil)
   (onresize :animatable nil)
   (onscroll :animatable nil)
   (onunload :animatable nil)
   (onerror :animatable nil)
   (timelinebegin :animatable nil)
   (view-box :attribute "viewBox")
   (zoom-and-pan :attribute "zoomAndPan")
   (xmlns :animatable nil :initform "http://www.w3.org/2000/svg")
   (version :status :deprecated)
   transform width height x y))

(define-svg-node svg-symbol (structural-element renderable-element never-rendered-element container-element)
  ((svg-aria-* :type svg-aria-*)
   (preserve-aspect-ratio :attribute "preserveAspectRatio")
   (ref-x :attribute "refX")   
   (ref-y :attribute "refY")
   (view-box :attribute "viewBox"))
  (:element . "symbol"))

(define-svg-node switch (renderable-element container-element)
  ((svg-aria-* :initarg :svg-aria-* :type svg-aria-* :reader svg-aria-*)
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)))

(define-svg-node text (text-content-element renderable-element graphics-element)
  ((svg-aria-* :initarg :svg-aria-* :type svg-aria-* :reader svg-aria-*)
   (length-adjust :attribute "lengthAdjust")
   (text-length :attribute "textLength")
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   dx dy rotate x y))

(define-svg-node text-path (text-content-child-element text-content-element renderable-element)
  ((svg-aria-* :initarg :svg-aria-* :type svg-aria-* :reader svg-aria-*)
   (length-adjust :attribute "lengthAdjust")
   (text-length :attribute "textLength")
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   (start-offset :attribute "startOffset")
   (xlink-href :attribute "xlink:href" :animatable nil)
   (xlink-title :attribute "xlink:title" :animatable nil)
   (svg-method :attribute "method")
   href path side spacing)
  (:element . "textPath"))

(define-svg-node svg-title (never-rendered-element descriptive-element)
  ()
  (:element . "title"))

(define-svg-node tspan (text-content-child-element text-content-element renderable-element)
  ((svg-aria-* :initarg :svg-aria-* :type svg-aria-* :reader svg-aria-*)
   (length-adjust :attribute "lengthAdjust")
   (text-length :attribute "textLength")
   (system-language :attribute "systemLanguage" :animatable nil)
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   dx dy rotate x y))

(define-svg-node use (structural-element renderable-element graphics-element graphics-referencing-element)
  ((svg-aria-* :initarg :svg-aria-* :type svg-aria-* :reader svg-aria-*)
   (required-extensions :attribute "requiredExtensions" :animatable nil)
   (system-language :attribute "systemLanguage" :animatable nil)
   (xlink-href :attribute "xlink:href" :animatable nil)
   (xlink-title :attribute "xlink:title" :animatable nil)
   use))

(define-svg-node view (uncategorized-element)
  ((svg-aria-* :type svg-aria-*)
   (view-box :attribute "viewBox")
   (zoom-and-pan :attribute "zoomAndPan")
   (preserve-aspect-ratio :attribute "preserveAspectRatio")))



;;; obsolete elements

(define-svg-node alt-glyph (text-content-child-element text-content-element)
  ()
  (:status . :obsolete)
  (:element . "altGlyph"))

(define-svg-node alt-glyph-def (text-content-element)
  ()
  (:status . :obsolete)
  (:element . "altGlyphDef"))

(define-svg-node alt-glyph-item (text-content-element)
  ()
  (:status . :obsolete)
  (:element . "altGlyphItem"))

(define-svg-node animate-color (animation-element)
  ()
  (:status . :obsolete)
  (:element . "animateColor"))

(define-svg-node cursor (uncategorized-element)
  ()
  (:status . :obsolete))

(define-svg-node svg-font (font-element)
  ()
  (:status . :obsolete)
  (:element . "font"))

(define-svg-node font-face (font-element)
  ()
  (:status . :obsolete))

(define-svg-node font-face-format (font-element)
  ()
  (:status . :obsolete))

(define-svg-node font-face-name (font-element)
  ()
  (:status . :obsolete))

(define-svg-node font-face-src (font-element)
  ()
  (:status . :obsolete))

(define-svg-node font-face-uri (font-element)
  ()
  (:status . :obsolete))

(define-svg-node glyph (text-content-element)
  ()
  (:status . :obsolete))

(define-svg-node glyphRef (text-content-element)
  ()
  (:status . :obsolete))

(define-svg-node hkern (font-element)
  ()
  (:status . :obsolete))

(define-svg-node missing-glyph (container-element)
  ()
  (:status . :obsolete))

(define-svg-node tref (text-content-child-element text-content-element)
  ()
  (:status . :obsolete))

(define-svg-node vkern (font-element)
  ()
  (:status . :obsolete))
