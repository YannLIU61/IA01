
(defun deriv-term (a b)
              (if(equal a b)
                  1
                  0))

                  
(defun deriv(expre var)
               (if (not(listp expre))
                   (deriv-term expre var)
                   (cond
                       ((eq '+ (car expre))(deriv-sum expre var) )
                       ((eq '* (car expre))(deriv-prod expre var) )
                    )
                )
               )

(defun deriv-prod (expre var)
               (list '+ (list '* (deriv (cadr expre) var) (caddr expre))  (list '* (cadr expre) (deriv (caddr expre) var) )
               )
               )                   

(defun deriv-sum(expre var)
               (if (not(listp expre))
                   expre
                 (list (car expre) (deriv(cadr expre) var) (deriv(caddr expre) var))
                 
                 
               ))

;;(print(deriv '(+ (* 3 x) (* 6 x)) 'x))