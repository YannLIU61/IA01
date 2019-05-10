
(defun inf2pref(L)
               (if (or(not(listp L))(not(cddr L)))
               L   
               (list (cadr L) (inf2pref(car L)) (inf2pref(caddr L)))
               )
                   )
(print (inf2pref '( (x + 2) / (x - 3))))