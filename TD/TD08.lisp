


(defparameter *frames* nil)

(setq ELEPHANT
      '(ELEPHANT
        (TYPE (VALUE . CONCEPT))
        (IS-A (VALUE . ETRE))
        (COLOR (DEFAULT . GREY))
        (AGE (IF-NEEDED ask-user)
             (IF-ADDED check-age))
        (POIDS (IF-NEEDED computer-weight-from-age))
        (AFFICHAGE (IF-ADDED draw-elephant)
                   (IF-REMOVED erase-elephant))
       )
)

(pushnew 'ELEPHANT *frames*)

;;(make-individu 'Clyde 'Elephant color "grey" age 5)
;; Example: un individu
; (CLYDE
;     (type (VALUE individu))
;     (is-a (VALUE ELEPHANT))
;     (color (VALUE GREY))
;     (age (VALUE 5))
; )




;;1. si le frame concept n'exist pas erreurs
;;2. creer un id unique
;;3. creer un début de frame avec le nom et individu
;;4. pour chaque slot .vérifier que le slot est autorisé  .si oui i faut créer slot avec la valeur correspondante est ajouter au début de frame précédent  
;;5. ajouter id à *frame* 
;;6. retourne id
;; pop_val => (color "grey" age 5)


(defun make-individu(name concept prop_val)
    (if (not (member concept *frames*)) ; si le concept n'est pas présent
        (format t "non initialisé" concept) ; on renvoit un message d'avertissement
        (let ( (new nil) (N (gentemp "N")) ) ; sinon on va effectivement créer l'individu
            (push (list 'type (cons 'value 'individu)) new)
            (push (list 'is-a (cons 'value concept)) new) ; on ajoute tout d'abord les slot IS-A et TYPE
            (loop
                (let ( (property (car prop_val)) (value (cadr prop_val)) )
                    (cond 
                       ( (numberp value)  (push (list 'age   (cons 'value value)) new) )
                       ( (Stringp value)  (push (list 'color (cons 'value value)) new) )
                    )              
                )
                (setq prop_val (cddr prop_val))
                (when (equal prop_val nil)  (return new)  )
            )  
            (set N new)
            (pushnew N *frames*)  
        )       
    )    
)
(defun make-individuP (name concept &rest prop-val)
        (unless (member concept *frames*) (error "Le concept n'existe pas"))
        (let ((id (gentemp "F")) (allowed-slots (mapcar 'car (cdr (symbol-value concept)))) slot value fn)
                (set id (list name (list 'type (list 'value 'individu)) (list 'is-a (list 'value concept))))
                (loop
                        (unless prop-val (return NIL))
                        (setq slot (pop prop-val))
                        (setq value (pop prop-val))                      
                        (when (member slot allowed-slots)
                                (setq fn (cadr (assoc 'if-added (cdr (assoc slot (cdr (symbol-value concept)))))))
                                (if fn
                                    (setq value (funcall fn slot value))
                                )
                                ;; (set value (if fn (funcall fn slot value)))
                                (when value
                                        (set id (append (symbol-value id) (list (list slot (list 'value value)))) )
                                )
                        )
                )
                (pushnew id *frames*)              
        )
)

(defun check-age (slot age)
    age
)


;;(setq val '(color "blue" age 5 ) )
(make-individuP 'Clyde 'ELEPHANT 'COLOR 'blue 'AGE 5)
(make-individuP 'Clyde2 'ELEPHANT 'AGE 7)
(print *frames*)
(print F1)
(print F2)


(defun get-slot-value (frame slot)
  ; on cherche la valeur du slot dans le frame
  ; frame : nom du frame
  ; slot : nom du slot
  ;1) SI le frame n'exist pas ->NIL
  ;2)Sinon
  ;     -s'il a une facette valeur pour le slot
  ;         on retourne cette val
  ;     -Sinon si default ds concept parent
  ;         on retourne la val
  ;     -sinon on essaie de retourne la val avec if-needed
  ;3)sinon on retourne la hiérachie
    ;; (if (not (member frame *frames*))
    ;;     (format t "~& ~a is not present as a frame" frame) ; on indique si le frame n'existe pas
    ;;     (let ((props (symbol-value frame)) val) ; on récupère toutes les slots
    ;;         (setq val  (cdr(cadr (assoc slot props)))) 
    ;;         val           
    ;;     )
    ;; )
     (unless (member frame *frames*) (error "Le concept ~S n'est pas"))
     (let ((get-slots (cdr (symbol-value frame))) val ) ; on récupère toutes les slots
        (if (setq val (cadr (assoc 'value (cdr (assoc slot get-slots))))) ; on regarde si la valeur est présente
            val   
            (get-slot-value-inherit (cadr (assoc 'value (cdr (assoc 'is-a get-slots)))) slot) ; sinon on regarde si elle peut être héritée


        )
     )
)

(defun get-slot-value-inherit (frame slot)
  ; on cherche une valeur hérité du slot présent le frame
  ; frame : nom du frame
  ; slot : nom du slot
    (if (not (member frame *frames*))
        nil ;arrêt de la récursion si l'on ne trouve pas
        (let ((props (cdr (symbol-value frame))) val)
            (if (setq val (cdr (assoc 'default (cdr (assoc slot props))))) ; on regard si la valeur est présente par défaut
                val
                (get-slot-value-inherit (cadr (assoc 'value (cdr (assoc 'is-a props)))) slot)
            )
        )
    )
) ; sinon on remonte


(print (get-slot-value 'F1 'COLOR) )
(print (get-slot-value 'F1 'AGE) )
(print (get-slot-value 'F2 'COLOR) )

