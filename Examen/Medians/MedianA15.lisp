;EXAMEN MEDIAN IA01 A15

;2. Le damier et les pièces d'or

;2.1 a)
(defvar *damier* '((9 5 2 8) (6 4 9 2) (1 0 8 3)))

;2.1 b)
(defvar NMAX 3)
(defvar MMAX 4)
;Attention : dotimes de 0 à count-1
;Le retour du premier dotimes ne sert à rien si pas de setq !!!
(defun nbPieces (case)
  (let ((nbp *damier*))
    (dotimes (i (pop case) (setq nbp (car nbp)))
      (print nbp)
      (pop nbp)
    )
    (print "lol")
    (dotimes (j (pop case) (car nbp))
      (print nbp)
      (pop nbp)
    )
  )
)

;2.2 a)
(defun successeurs (case)
  (let ((suc nil))
    (if (NOT (= (car case) (- NMAX 1)))
      (push (list (+ (car case) 1) (cadr case)) suc)
    )
    (if (NOT (= (cadr case) (- MMAX 1)))
      (push (list (car case) (+ (cadr case) 1)) suc)
    )
    suc
  )
)

;2.2 b)
;Attention : member :test 'equal !!!
;Modifier gain & chemin dès le début pour ne rien oublier !!!
(defun explore (case gain chemin)
  (push case chemin)
  (setq gain (+ gain (nbPieces case)))
  (if (member (list (1- NMAX) (1- MMAX)) chemin :test 'equal)
    (print (setq chemin (cons gain (reverse chemin))))
    (dolist (x (successeurs case))
      (explore x gain chemin)
    )
  )
)

;2.2 c)
(defun debut-explore () (explore '(0 0) 0 nil))

;2.3
(defvar *chemin* nil)
;Version variable globale
(defun explore (case gain chemin)
  (push case chemin)
  (setq gain (+ gain (nbPieces case)))
  (if (member (list (1- NMAX) (1- MMAX)) chemin :test 'equal)
    (defparameter *chemin* (append *chemin* (list (setq chemin (cons gain (reverse chemin))))))
    (dolist (x (successeurs case))
      (explore x gain chemin)
    )
  )
)

(defun gainMax ()
  (let ((max 0))
    (dolist (x *chemin* (assoc max *chemin*))
      (if (> (car x) max)
        (setq max (car x))
      )
    )
  )
)

;3. Cartes de fidélité

;3.1
(defvar *cartes* '(
  (c15101901 Vador (Arme 10) (ForceObscure 5) (ForceLumineuse 3))
  (x98921823 Anakin (Arme 8) (ForceObscure 3) (ForceLumineuse 8))
  (o12330318 Yoda (Arme 9) (ForceObscure 0) (ForceLumineuse 7))
  (i13454834 DarkMaul (Arme 6) (ForceObscure 7) (ForceLumineuse 2))
))

;3.2
(defvar *produits* '(
  (SabreLaserRouge 850 Arme (forceObscure extraPuissante))
  (V-19 65000 vaisseau (forceObscure tresRapide))
  (SabreLaserBleu 850 Arme (forceLumineuse extraPuissante))
))

;3.3
(defvar panierClient_c15101901 '((V-19 1) (SabreLaserRouge 100)))

;3.4
(defun produit (nomProduit listeProduits)
  (assoc nomProduit listeProduits)
)

(produit 'SabreLaserRouge *produits*)
;(SABRELASERROUGE 850 ARME (FORCEOBSCURE EXTRAPUISSANTE))

;3.5
(defun ListeCa (typeInfo nomProduit)
  (cond
    (
      (equal typeInfo 'Categorie)
      (caddr (produit nomProduit *produits*))
    )
    (
      (equal typeInfo 'Caracteristiques)
      (car (last (produit nomProduit *produits*)))
    )
  )
)

(ListeCa 'Categorie 'SabreLaserRouge)
;ARME

;3.6
(defun CalculScore (panier cartes produits typeInfo typeCa)
  (let ((score 0))
    (dolist (x panier score)
      (if (equal typeCa (ListeCa typeInfo (car x)))
       (setq score (+ (1+ score) (cadr x)))
      )
    )
  )
)
;Exemple de l'énoncé faux ??
;Vador a 100x SabreLaserRouge qui sont Arme donc :
(CalculScore panierClient_c15101901 *cartes* *produits* 'Categorie 'Arme)
;101


;3.7
(defun Liste_Empire (cartes score1 seuil1 score2 seuil2)
  (let ((siths nil))
    (dolist (x cartes siths)
      (if (AND (> (cadr (assoc score1 (cddr x))) seuil1)
               (> (cadr (assoc score2 (cddr x))) seuil2))
        ;ATTENTION : on ne peut pas faire (assoc score x)
        ;car x contient des atomes auxquels on ne peut pas
        ;appliquer un car, par contre (cddr x) est bien
        ;consitué uniquement de listes.
        (push (cadr x) siths)
      )
    )
  )
)

(Liste_Empire *cartes* 'forceObscure 3 'Arme 5)
;(DARKMAUL VADOR)
