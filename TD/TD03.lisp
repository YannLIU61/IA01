;;; Compléments au TD2 : simplication d'expression

(defun simplificationTerme (exp)
  (cond
    ((and
      (eq (car exp) '*)
      (or (eq (cadr exp) 0) (eq (caddr exp) 0))
     ) 0) ; élément absorbant de la multiplication
    ((and
      (numberp (cadr exp)) (numberp (caddr exp))
      ) (eval E)) ; cas de deux nombres
    ((and
      (eq (car exp) '*) (or (eq (cadr exp) 1) (eq (caddr exp) 1))
      )(if (eq (cadr exp) 1)
        (caddr exp)
        (cadr exp))
    ) ; élement neutre de la mutliplication
    (T exp) ; permet de ne pas renvoyer NIL mais plutôt exp si il n'y a rien à simplifier
    )
)

; Version récursive
(defun simplification (E)
  (if (atom E) ; si l'expression est atomique
    E ; on l'évalue
    (simplificationTerm (list (car E) (simplification (cadr E)) (simplification (caddr E)))) ; sinon on la simplifie
    ; à l'aide de la fonction précédente
  )
)

(setq ll '(a 1 bb 2 ccc 3 ddd 4))

(defun F1 (l)
    (loop for n in l
    do(print n)
    )
)
(defun F11 (l)
   (when (and (cdr l) (car l)) (print (car l)) (F11 (cdr l)))
   (if (not (cdr l )) (print l))

    

)

(defun F2(l)
    (dolist (x l) 
        (print x)
    )
)

(defun F3(l)
    (mapcar #'(lambda(a) (print a)) l)  ;; (mapcar 'print l)
)

(defun F4(l)
    (dotimes (x (length l) 'fin)
    (print (car l))
    (setq l (cdr l))
    )
)

(defun F6(l)
    (print (car l))
    (if (cdr l)
        (F6 (cdr l))
        NIL
    )
)

(defun F7(l)
    (maplist #'(lambda(x) (print (car x))) l)
)
;(F1 ll)
(F11 ll)