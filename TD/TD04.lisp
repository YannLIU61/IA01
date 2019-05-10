;;labyrinthe
;;        1
;;1)ensemble d'etats:{1,2....20}U{entree sortie}
;;2)initiaux:{entree}
;;3)solution:{sortie}
;;4)actions:deplacement

;; 2
(setq *lab* '(
    (entree 1) 
    (1 entree 2) 
    (2 1 7) 
    (3 6) 
    (4 5) 
    (5 4 12)
    (6 3 7) 
    (7 2 6 8) 
    (8 7 9) 
    (9 8 10) 
    (10 9 11 15)
    (11 10 12 14) 
    (12 5 11 14) 
    (13 20)
    (14 11) 
    (15 10 16) 
    (16 15 17) 
    (17 16 18) 
    (18 17 19) 
    (19 18 20) 
    (20 19 sortie) 
    (sortie 20)
    ) 
)
(print *lab*)
;; (member 'c '( a b c d e))
;; >(c d e)
;; (push 'b '(a b c d))
;; >(b a b c d)
;; (pushnew 'b '( a b c d))
;; >(a b c d)

;; (assoc 'c '( (a 1) ( b 2) (c 3 4)))
;; >(c 3 4)

(defun successeurs (etat lab)
    (cdr(assoc etat lab))
)

(defun successeurs_valides (etat lab deja_visites)
    (let ((successeurs (successeurs etat lab)) (retour '()))
        (dolist (node successeurs)
            (if (not (member node deja_visites))
            (push node retour)
            )
        )
        (reverse retour)
    )
)

 (setq *visitesd* '())
(defun explore_dfs (etat lab chemin)
    (pushnew etat *visitesd* )
    (if (eq etat 'sortie)
        (print chemin)
        (let ((successeurs (successeurs_valides etat lab *visitesd* ) ))
            (dolist (node successeurs)          
                 (explore_dfs node lab (append chemin (list node)) )                     
            )
        )   
    )
)
 (explore_dfs 'entree *lab* '())

;;  (defun explore-prof (etat sortie lakb cheminParcours)
;;     (let (()))
 
 
 
;;  )

;; (defun explore-larg (etats sortie laby cheminParcours)
;; (let ((suivanys nil) (sol nil)))
;; (format t "Explore etats ~s, cheminP  ~s~%" etats cheminParcours)
;; (cond 
;;     ((member sortie etats) (return-from explore-larg (liste sortie) ))
;;     ((not etats ) (return-from ))



;; )



;; )

;; (defun explore (etat old_states)
;;     (cond ((null state) nil)
;;           ((eq etat 'sortie) "succes")
;;           ((explore (get_new_state state old_states *lab*)  (cons state old_states)))
;;           (t (backtrack (car old_states)(cdr old_states) *lab*))
;;     )
;; )

;; (defun backtrack (state old_states *lab*)
;;     (let (candidate)
;;     (cond 
;;         ((null state) nil)
;;         ((setq candidate (cadr (member state (successeurs (car old_states) *lab*)))))
;;         (explore candidate old_states)
;;     )
;;     (t (backtrack (car old_states)(cdr old_states) *lab*))


    
    
;;     )


;; )

;; (defun explore (state oldStates)
;;   ; permet d'explorer l'arbre à partir d'un état actuel state et
;;   ; de la liste des états précédement visité oldStates
;;   (print (list 'state state)) ; on montre où l'on en est
;;   (read-char) ; permet la pose
;;   (cond
;;     ((null state) nil) ; si on ne peut plus descendre, on renvoit nil
;;     ((eql state 'exit) 'sucess) ; sinon, si on est à la sortie on a fini !
;;     ; sinon on ajoute le noeud actuel au ancien et on avance en profondeur :
;;     ((explore (getNewState state oldStates *lab*)(cons state oldStates)))
;;     (t (backtrack state oldStates *lab*)) ; sinon on ne peut rien faire on remonte à l'état précédent
;;   )
;; )

;; (defun getNewState (state oldStates *lab*)
;;   ; retourne un nouvel état fils de state et non déjà visité
;;   ;;; A CONSTRUIRE : surement utiliser une variable globale L pour enregistrer
;;   ;;; les états non visités et faire quelque chose comme
;;   ;;; (car (intersection L (successeurs state)))
;; )

;; (defun candidate (state oldStates *lab*)
;;   ; retourne le candidat i.e le noeud juste après l'état en cours (ie state)
;;   ; et qui n'a pas encore été traité
;;   (cadr (member (state (successeurs (car oldStates) *lab*))))
;; )
;; (defun backtrack (state oldStates *lab*)
;;   ; permet de remonter dans l'arbre à partir d'un état actuel state et
;;   ; de la liste des états précédement visité oldStates
;;   (cond
;;     ((null state) nil) ; si on ne peut plus remonter, on renvoit nil
;;     ((explore (candidate state oldStates *lab*) oldStates)) ; sinon en explore un nouveau noeud candidat
;;     (t (backtrack (car oldStates) (cdr (oldStates) *lab*))) ; sinon on ne peut rien faireon remonte à l'état précédent
;;   )
;; )




