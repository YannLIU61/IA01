;;  TD06 Representation NETL



;; noeud ( (name . #) (type . #) (inarcs . # ) (outarcs . #) )
;; arcs ( (type . #) (from . #) (to . #) )
;; EX: N0 ( (name . Roxane) (type . individu) (inarcs . A0) (outarcs . A1) )
;;     N1 ( (name . Christiane) (type . individu) (inarcs . A1) (outarcs . A0) )
;;     A0 ( (type . aime) (from . N0) (to . N1) )
;;     A1 ( (type . aime) (from . N1) (to . N0) )
;;      (gentemp "N") ==> N0 N1 N2 N3

(defvar *noeuds* nil)
(defvar *arcs* nil)

(defun defnode (name type)
    (let ((N (gentemp "N")))
        (set N (list (cons 'name name) (cons 'type type)) ) ;; ici utilise set, si utilise setq il va push la liste dans *noeuds*.
        (pushnew N *noeuds*)
        N       
    )
)

;; (defun defarc (type from to)
;; ;; si les noeuds from et to n'exist pas -> erreur
;; ;; creer de l'arc
;; ;;mise a jour de in-arcs et out-arcs dans from et to
;; ;;maj de *arcs*
;; ;;retourne l'id de l'arc
;;     (if (null from) (error "Noeud vide...")) ; gestion des erreurs
;; 	(if (null to) (error "Noeud vide...")) ; gestion des erreurs
;;     (let ((A (gentemp "A")))
;;             (set A (list (cons 'type type) (cons 'from from) (cons 'to to) ) ) 
;;             (pushnew A *arcs*)
;;             A       
;;         )
;; )
;;version du cours

(defun defarc (type from to)
    (cond   ( (not (and (member from *noeuds*) (member to *noeuds*))) 
                (error "Noeud non existant")
            )
            (T (let ( (A (gentemp "A")))
                (set A (list (cons 'type type) (cons 'from from) (cons 'to to) ))
                (push A *arcs*)
                A
            ))  
    )
)

(defnode 'LIU 'Man) 
(defnode 'Yan 'Man) 
(defarc 'aime 'N1 'N2)

;; ((NAME . LIU) (TYPE . MAN)) 
;; ((NAME . YAN) (TYPE . MAN)) 
;; ((TYPE . AIME) (FROM . N1) (TO . N2))


;; implementation du TD06
;;
(defun get_prop_val (id prop)
    (cdr (assoc prop (symbol-value id))) ;; symbol-value get valeur du id (comme eval)
)

;; (defun set_prop_val (id prop val)
;; ;;donne à prop la valeur val dans id
;;     (let ( (paire ((assoc prop (symbol-value id))))
;;         (set id (cons (cons prop val)
;;         (remove paire (symbol-value id) :test #' equal)
;;         ))   
;;     )
;; )

(defun set_prop_val (id prop val)
    (setf 
       (cdr (assoc prop (symbol-value id)));; peut pas directement appele get_prop_val()
        val
    )
)

(defun add_prop_val (id prop val)
;;ajoute la valeur val a prop
    (let ((pair (assoc prop (symbol-value  id))))
        (if pair   ;; tester si prop est exist.
            (set_prop_val id prop (cons val (cdr pair)))
            (push (cons prop val) (symbol-value id))
        )
    )
)

(print( get_prop_val 'N1 'name))
(set_prop_val 'N1 'name 'LIUYan)
(add_prop_val 'N1 'age '23)

(print N1)
(print N2)
(print A3)
;; LIU 
;; ((AGE . 23) (NAME . LIUYAN) (TYPE . MAN)) 
;; ((NAME . YAN) (TYPE . MAN)) 
;; ((TYPE . AIME) (FROM . N1) (TO . N2)) 

(defun get-marked-nodes (mark)
  ; pour obtenir les noeuds marqué selon mark dans le réseau
    (let ((res))
        (dolist (x *noeuds* (reverse res))
            (if (equal (get 'age x) mark)
                (push x res)
            )
        )       
    )   
)
(print (get-marked-nodes 'age))