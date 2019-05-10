
; source lastword is ;, alors python last-word probability 20%

(defclass $result ()
    (
        (c :accessor ?c :initarg :c :initform NIL)
        (cplusplus :accessor ?cplusplus :initarg :cplusplus :initform NIL)
        (python :accessor ?python :initarg :python :initform NIL)
        (java :accessor ?java :initarg :java :initform NIL)
        (javascript :accessor ?javascript :initarg :javascript :initform NIL)
        (lisp :accessor ?lisp :initarg :lisp :initform NIL)
    )
)

(defclass $probability ()
    (
        (lastcharacter-probability :accessor ?lastcharacter-probability :initarg :lastcharacter-probability :initform 0)
        (firstword-probability :accessor ?firstword-probability :initarg :firstword-probability :initform 0)
        (punctualition-probability :accessor ?punctualition-probability :initarg :punctualition-probability :initform 0)
        (bracket-probability :accessor ?bracket-probability :initarg :bracket-probability :initform 0)
        (operator-probability :accessor ?operator-probability :initarg :operator-probability :initform 0)
        (motcle-probability :accessor ?motcle-probability :initarg :motcle-probability :initform 0)
    )
)

(defclass $source-descriptor ()
    (
        (lastcharacter :accessor ?lastcharacter :initarg :lastcharacter)
        (firstword :accessor ?firstword :initarg :firstword)
        (punctualition-rate :accessor ?punctualition-rate :initarg :punctualition-rate)
        (bracket-rate :accessor ?bracket-rate :initarg :bracket-rate)
        (operator :accessor ?operator :initarg :operator)
        (motcle :accessor ?motcle :initarg :motcle)
    )
)

(defparameter source (make-instance '$source-descriptor :lastcharacter (intern ";")))
(defparameter result (make-instance '$result))

(defparameter languages '())
(defparameter c (make-instance '$probability))
(push c languages)
(defparameter cplusplus (make-instance '$probability))
(push cplusplus languages)
(defparameter python (make-instance '$probability))
(push python languages)
(defparameter java (make-instance '$probability))
(push java languages)
(defparameter javascript (make-instance '$probability))
(push javascript languages)
(defparameter lisp (make-instance '$probability))
(push lisp languages)

(setq regles '(
        (R1 (
                (equal (?lastcharacter source) (intern ";"))
            )
            (

                (equal (lastcharacter-probability c) 1) 
                (equal (lastcharacter-probability cplusplus) 1)
                (equal (lastcharacter-probability java) 1)
                (equal (lastcharacter-probability javascript) 1)
            )
        )
        (R2 (
                (equal (?lastcharacter source) (intern ")")) 
            )
            (
                (equal (lastcharacter-probability lisp) 1) 
                (equal (lastcharacter-probability python) 1)
                (equal (lastcharacter-probability javascript) 0.5)
            )
        )
        (R100   ; Example for merging
            (
                (> (+ 
                    (?lastcharacter-probability c)
                    (?firstword-probability c)
                    (?punctualition-probability c)
                    (?bracket-probability c)
                    (?operator-probability c)
                    (?motcle-probability c)) 
                    3 ; Over 50%
                )
            )
            (
                (equal (c result) 'T) 
            )
        )
        

    )
)

(defun nom (regle)
    (car regle)
)

(defun premisses (regle)
    (cadr regle)
)

(defun conclusions (regle)
    (caddr regle)
)

(defun appartient (item items)
    (dolist (sub-item items)
        (if (equal sub-item item)
            (return T)
            NIL
        )
    )
)

(defun RegleCanditat (but regles)     
    (if (null regles) 
        NIL         
        ()
    ) 
) 

;(print (appartient '(setf (?lastcharacter-probability c) 0.5) (caddar regles)))

;(print (equal (?lastcharacter source) (intern ";")))
