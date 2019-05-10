;;; CHAINAGE AVANT

(defun ChainageAvantProf (but)
  (let (EC (BR *BR*) (BF *BF*) regleCourante)
    (loop ; on boucle
      (if (presence but BF) ; si le but est présent dans la base de faits,
        (return "Vérifiée !") ; on sort !
        (dolist (r BR) ; sinon on parcourt les règles dans la base de règles
          (when (declenchable? r BF); si une règle est déclenchable
            (push r EC) ;on l'ajoute à l'ensemble contraint AU DEBUT
            ;en réalité cela ne permet pas de faire une réelle recherche
            ;en profondeur car le push détruit l'ordre si plus d'une règle
            (setq BR (remove r BR)) ; on l'enlève de la base de règles
          )
        )
      )
      (if EC ; si on peut encore déclencher des règles
        (progn
          (setq regleCourante (pop EC)) ; on choisit la dernière obtenue
          (pushnew (conclusion regleCourante) BF) ; on ajoute son résultat à la base de faits
        )
        (return "Non vérifiée ...")
      )
    )
  )
)

(defun ChainageAvantLarg (but)
  (let (EC (BR *BR*) (BF *BF*) regleCourante)
    (loop ; on boucle
      (if (presence but BF) ; si le but est présent dans la base de faits,
        (return "Vérifiée !") ; on sort !
        (dolist (r BR) ; sinon on parcourt les règles dans la base de règles
          (when (declenchable? r BF); si une règle est déclenchable
            (setq EC (append EC (list r))) ; on l'ajoute à l'ensemble contraint EN FIN
            (setq BR (remove r BR)) ; on l'enlève de la base de règles
          )
        )
      )
      (if EC ; si on peut encore déclencher des règles
        (progn
          (setq regleCourante (pop EC)) ; on choisit la dernière obtenue
          (pushnew (conclusion regleCourante) BF) ; on ajoute son résultat à la base de faits
        )
        (return "Non vérifiée ...")
      )
    )
  )
)

;;Fonctions outils
(defun verifiee? (r faits)
  (member r faits))

(defun conclusion (r)
  (cadr r))

(defun declenchable? (r faits)
  (let ((OK t))
    (dolist (p (car r) OK)
      (if (not (presence p faits))
        (setq OK nil)
      )
    )
  )
)

;; CHAINAGE ARRIERE
(defun ChainageArriereProf (but)
  (if (verifier but)
    "Vérifiée !"
    "Non vérifiée ..."
  )
)

(defun candidates (but)
  (let (res)
    (dolist (x *BR* (reverse res)) ; on parcout la base de règle ; on renverra la liste res
      (if (member but (last x)) ; si le but est présent dans les résultas d'une règle
          (push (list (car x)) res) ; on ajoute l'indice de la règle à la liste res
      )
    )
  )
)

(defun premisses (regle)
  (cadr (assoc regle *BR*)) ; retourne les prémisses d'une règle
)

(defun verifier (p)
  (let ((OK nil) EC R)
    (if (member p *BF*)
      (setq OK t)
      (progn
        (setq EC (candidates p))
        (while (and (not OK) EC)
          (setq R (pop EC))
          (setq OK (verifierRegle r))
        )
      )
    )
  OK
  )
)

(defun verifierRegle (R)
  (let ((OK T) (PREM (premisses R)) P)
    (while (and OK PREM)
      (setq P (pop PREM))
      (setq OK (verifier P))
    )
    OK
  )
)


;; Recherche dans un graphe d'états

(defun largeur (file)
  ; File : liste de (etat etatsVisites)
  (when file ; s'il y a quelque chose dans la file, alors
    (let* ( ; déclarations
        (paire (pop file)) ; on récupère l'élément de la file
        (etat (car paire)) ; on récupère l'état à traiter
        (etatsVisites (cadr paire)) ; on récupère les états visités
        (listSucc (successeurs etat etatsVisites)) ; on construit la liste des successeurs
      )
      (if (member etat *F* :test 'equal) ; si F est dans les états finaux
        (push (reverse (cons etat etatsVisites)) solutions)
        (dolist (s listSucc) ; pour chacun de ses successeurs
          (setq file (append file (list (list s (cons etat etatsVisites))))); ajout de la paire (s (cons etat etatsVisites)) à la file
        )
      )
      (largeur file) ; on traite la file
    )
  )
)

(defun profondeur (etat etatsVisites)
  ; etat : état courant à traiter ; etatsVisites : liste des états déjà visités
  (if (member etat *F* :test #'equal) ; si l'état fait partie de F
    (push (reverse etatsVisites) solutions) ; on ajoute le chemin aux solution
    (progn ; sinon
      (setq listSucc (successeurs etat etatsVisites)) ; on récupère les successeurs de etat
      (dolist (s listSucc) ; on traite chacun de ces successeurs
        (profondeur s (cons etat etatsVisites)) ; en rajoutant etat aux états visités
      )
    )
  )
)

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
(defun successeurs (etat etatsVisites)
  ; etat : liste pointée représentant un état (x y)
  ; etatsVisites : liste d'états déjà visités
  (let (listSucc (listeActions (actions etat)))
    (dolist (A listeActions) ; pour chaque actions applicables
      (push (appliquerAction etat A) listSucc) ; on récupère les états résultats
    )
    (dolist (S listSucc listSucc) ;  pour chacun des successeurs potentiels
      (if (member S etatsVisites :test #'equal) ; si l'état a déjà été visité
        (setq listSucc (remove S listSucc)) ; on l'enlève des états successeurs
      )
    )
  )
)

(setq *BF* '(A B G J))
(setq *BR* '(
  ((A) C)
  ((C) E)
  ((E) F)
  ((A B) D)
  ((F G) I)
  ((J I E) X)
  ((F D) J)
  )
)
