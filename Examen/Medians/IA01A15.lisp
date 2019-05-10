(defun nbPieces (case)
  (let ((liste *damier*) x)
    (do ((x 0 (+ x 1))) (= x (car case))
      (pop liste)
    )
    (setq liste (pop liste))
    (do ((x 0 (+ x 1))) (= x (cadr case))
      (pop liste)
    )
    (pop liste)
  )
)




(defun successeurs (case)
    (cond
     ((and (> (car case) (- NMAX 2)) (> (cadr case) (- MMAX 2)))
      nil)
     ((and (< (car case) (1- NMAX))  (= (cadr case) (1- MMAX)))
      (list (list (1+ (car case)) (cadr case))))
     ((and (= (car case) (1- NMAX))  (< (cadr case) (1- MMAX)))
      (list(list (car case) (1+ (cadr case)))))
     (t (list (list (car case) (1+ (cadr case))) (list (1+ (car case)) (cadr case)))
     )
     )
)


(defun explorer (case gain chemin)
   (if (and (eq (car case) (1- NMAX)) (eq (cadr case)(1- MMAX)))
       (progn
         (pushnew case chemin)
         (setq chemin (reverse chemin))
         (pushnew (+ gain (nbpieces Case)) chemin)
         (pushnew chemin *CHEMINS*)
         )
       (dolist (x (successeurs case))
        (explorer x (+ gain (nbpieces case))(push case chemin))
       )
    )
)

(defun parcourir ()
  (setq *CHEMINS* '())
    (explorer '(0 0) 0 '())
  *chemins*
)


(defun nbpieces (c)
               0)

(defun gainmax ()
  (let ((liste (parcourir)) (max 0))
    (dolist (x liste max)
      (if (> (car x) max)
        (setq max (car x))
      )
    )
    (assoc max liste)
  )
)

;; Exercice 3 : cartes de fidélité

;1.

(setq *cartes* '(
  (c15101901 DarkVador (50 50 0))
  (c14701923 Yoda (50 0 50))
  (c14714317 R2D2 (50 25 25))
)
)

;2.

(setq *produits* '(
  (SabreLaserRouge 42 Arme (forceObscure extraPuissante))
  (SabreLaserBleu 31 Arme (forceLumineuse tresRapide))
  (DoubleSabreVert 150 Arme (forceLumineuse extraPuissante moyenRapide))
  (v-19 1000 vaisseau (forceObscure tresRapide))
  )
)

;3.

(setq panierClient_C15101901 '(DarkVador (
                                            (SabreLaserRouge 150)
                                            (V-19 1)
                                          )
                              )
)

;4.
(defun produit (p)
  (assoc p *produits*)
)

;5.
(defun listeCA (choix p)
  (case
    ((eq choix 'Categorie) (caddr (produit p)))
    ((eq choix 'Caracteristiques) (cadddr (produit p)))
    (t nil)
  )
)

;6.
(defun CalculScore (panier *cartes* *produits* choix1 choix2)
  (let ((scores (caddr (assoc (car panier) *cartes*))))
    (cond
      ((eq choix1 'Categorie)
        (dolist (x (cdr panier) (car scores))
          (if (and (eq choix2 'Arme) (eq choix2 (listeCA choix2 (produit x))))
            (setq (car scores) (1+ (car scores)))
          )
        )
      )
      ((eq choix1 'Caracteristiques)
        (cond
          ((eq choix2 'forceObscure)
            (dolist (x (cdr panier) (cadr scores))
              (if (eq choix2 (ListeCA choix2 (produit x)))
                (setq (cadr scores) (1+ (cadr scores)))
              )
            )
          )
          ((eq choix2 'forceLumineuse)
            (dolist (x (cdr panier) (caddr scores))
              (if (eq choix2 (ListeCA choix2 (produit x)))
                (setq (caddr scores) (1+ (caddr scores)))
              )
            )
          )
          (t nil)
        )
      )
      (t nil)
    )
  )
)
