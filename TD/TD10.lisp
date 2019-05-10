(defclass $Point ()
    (
        ($x :accessor ?x :initarg :x )
        ($y :accessor ?y :initarg :y )
    )
)

;; (defclass $Carre ($Point)
;;     (($w :accessor w?   :initarg :w))
;; )

(defclass $Carre()
    (
        ($Sommet :type $Point   :accessor ?Sommet   :initarg :Sommet)
        ($w :accessor ?w   :initarg :w)
    )
)

(defclass $Rectangle($Carre)
    (
        ($h :accessor ?h    :initarg :h)
    )
)

(defclass $Cercle ($Point)
    (
        ($r :accessor ?r    :initarg  :r)
    )
)

(defclass $Triangle ()
    (
        ($M1 :type $Point   :accessor ?M1   :initarg :M1)
        ($M2 :type $Point   :accessor ?M2   :initarg :M2)
        ($M3 :type $Point   :accessor ?M3   :initarg :M3)
    )
)

(defclass $Polygone ()
    (
        ($sommets   :type list  :accessor ?sommets  :initarg :sommets)
    )
)

(defmethod =translate ((p $Point) dx dy)
  (setf (?x p) (+ dx (?x p)))
  (setf (?y p) (+ dy (?y p)))
  (describe p)
)

(defmethod =translate((t $Triangle) dx dy)
    (=translate (?M1 t) dx dy)
    (=translate (?M1 t) dx dy)
    (=translate (?M1 t) dx dy)
)

(defmethod =translate((p $Polygone) dx dy)
   (dolist (m (?sommets p))
    (=translate m dx dy)
   )
)

(defmethod =symx ((p $Point))
  (setf (?y p) (- 0 (?y p)))
  (describe p)
)

(defmethod =symy ((p $Point))
  (setf (?x p) (- 0 (?x p)))
  (describe p)
)

(defmethod =symO ((p $Point))
  (=symx p)
  (=symy p)
  (describe p)
)


