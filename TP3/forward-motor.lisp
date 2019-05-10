(load "regles.lisp")

(defun valid-premisse (premisse)
    (funcall (car premisse) (eval (cadr premisse)) (eval (caddr premisse)))
)

(defun apply-conclusion (conclusion)
    (setf (slot-value (eval (cadadr conclusion)) (caadr conclusion)) (eval (caddr conclusion)))
)

(defun valid-et (premisses)
    (let ((result T))
        (dolist (premisse premisses)
            (if (not (valid-premisse premisse))
                (setf result NIL)
            )
        )
        result
    )
)

;; Breadth first search, O(n)
(defun parcour-regles (regles)
    (dolist (regle regles)
        (format T "Validating Regle ~A" (nom regle))
        (format T " Result: ~A ~%" (valid-et (premisses regle)))
        (if (valid-et (premisses regle))
            (dolist (conclusion (conclusions regle))
                (apply-conclusion conclusion)
            )
        )
    )
)

(print (valid-et (premisses (car regles))))
;(parcour-regles regles)

