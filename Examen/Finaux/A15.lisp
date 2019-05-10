#| IA01 - A15 |

II. Système expert d'ordre 1
; Base de fait initiale :
(BF
  (f1 (type lieu) (nom reception) (connexions bureau entree))
  (f2 (type lieu) (nom bureau) (connexions reception entree))
  (f3 (type lieu) (nom entree) (connexions reception bureau garage magasin depot))
  (f4 (type lieu) (nom garage) (connexions entree))
  (f5 (type lieu) (nom magasin) (connexions depot entree))
  (f6 (type lieu) (nom depot) (connexions magasin entree))
  (f7 (type objet) (nom C1) (presence magasin))
  (f8 (type objet) (nom C2) (presence bureau))
  (f9 (type robot) (nom R1) (presence entree) (brasLibres t) (objet C1)(destination depot) (tacheFinie nil))
  (f10 (type robot) (nom R2) (presence bureau) (brasLibres t) (objet C2)(destination reception) (tacheFinie nil)))

; On représente une règle de cette façon :

; (Ri ( () () )    ( () () ))
;     premisses    conclusions

(BR
  (R1
        ( ((type ?f) == robot) ((presence ?f) == (destination ?f)) ((tacheFinie ?f) == nil) )
        ( ((brasLibres ?f) = nil) ) )
  (R2
        ( ((type ?f) == robot) ((brasLibres ?f) == t) ((tacheFinie ?f) == nil) ((presence (objet ?f)) in (connexions (presence ?f))) )
        ( ( (presence ?f) = (presence (objet ?f))) ) )
  (R3
        ( ((type ?f) == robot) ((presence ?f) == (destination ?f)) ((tacheFinie ?f) == nil) )
        ( ((brasLibres ?f) = nil) ) )
  (R4
        ( ((type ?f) == robot) ((presence ?f) == (destination ?f)) ((tacheFinie ?f) == nil) )
        ( ((brasLibres ?f) = nil) ) )
  (R5
        ( ((type ?f) == robot) ((presence ?f) == (destination ?f)) ((tacheFinie ?f) == nil) )
        ( ((brasLibres ?f) = nil) ) )
  (R6
        ( ((type ?f) == robot) ((presence ?f) == (destination ?f)) ((tacheFinie ?f) == nil) )
        ( ((brasLibres ?f) = nil) ) )



III . Les frames : variante du jeu Agar.io

(Boule1 (nom (Boule1)) (taille (6)) (couleur Rouge))
(Boule2 (nom (Boule2)) (taille (8)) (couleur vert))
(Boule3 (nom (Boule3)) (taille (5)) (couleur Rouge))
(Boule4 (nom (Boule4)) (taille (3)) (couleur Rouge))
(Boule5 (nom (Boule5)) (taille (5)) (couleur Vert))

1. Frame du concept BOULE

|#

(BOULE
  (nom ($default (Boule)))
  (type ($value concept))
  (taille ($defaut 5))
  (vitesse ($if-needed calculer-vitesse))
  (puissance ($if-needed calculer-puissance))))

#|
  2. Fonction / demon calcule-puissance
  |#

(defun calcule-puissance (boule EB)
  (let ((laBoule (cdr (assoc boule EB))))
  (if laBoule
    (apply '+ (cadr (assoc 'taille laboule)))
    0)))

(

(defun get-slot-value (boule slot bd oboule)
  (if boule
    (let (val (contenuSlot (assoc slot (cdr (assoc boule bd)))))
    (if (not oboule)
      (setq oboule boule))
      (cond
        ((setq val (cadr contenuSlot))
          val)
        (t (get-slot-value (papaframe boule) slot bd oboule))))
        nil))


(defun fusion (b1 b2 bd)
  (let (newBoul
        (n1 (get-slot-value b1 'nom bd nil))
        (n2 (get-slot-value b2 'nom bd nil))
        (T1 (get-slot-value b1 'taille bd nil))
        (T2 (get-slot-value b2 'taille bd nil))
        (C (get-slot-value b1 'couleur bd nil)))
        (print n1)
    (push (setq newBoul `((nom (,@n1 ,@n2) (taille (,@t1 ,@t2)) (couleur ,C)))) bd)
    (remove (assoc (car n1) bd) bd :test 'equal)
    (remove (assoc (car n2) bd) bd :test 'equal)))
