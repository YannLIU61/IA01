;; Médian IA01 A09

;; Exercice 1 :

;1- 
    ;-Approche Sympbolique: étudier et imiter le raisonnement
    ;    Manipulation d'expressions mettant en oeuvre un processus de raisonnement
    ;    Nécéssite une représentation explicite des connaissances et méthodes de résolution du problème
    ;    Effort important de programmation
        
    ;-Approche Numérique: vise la perception et fonctionnement reflexe
    ;    Construction de modèles informatiques
    ;    Exploitation de processus d'apprentissage
    ;    Dev. de systèmes adaptatifs et évolutifs
    ;    Nécéssite peu ou pas de programmation
        
    ;-Approche Hybride: utilisation complémentaires des approches Symbolique et Numérique
    
    ;-Approche Distribuée: Vision de la pensée comme phénomène collectif
    ;    Systèmes multi-agents
        
;2-
    ;Représentation des connaissances: Support aux traitements que l'on souhaite effectuer sur ces connaissances (orgnaniser, classer, chercher, extraire...)
    ;Se rattache à une approche Symbolique de l'IA.
    
;3-
    ;Exemple de Formalisme de représentation:
    ;   Les Réseaux Sémantiques:
    ;       Ensemble de noeuds et d'association représentées par des liens représentant le sens des noeuds
    ;       Graphe orienté acyclique dont les noeuds et arcs sont étiquetés
    ;       Graphe d'héritage (génralisation/spécialisation)
    ;           ex: NETL

;4-
    ;Caractéristiques du language LISP
    ;   Programmation Interactive, Fonctionnelle (modularité, découpage en fonction, fort usage de la recursivité) et Symbolique (creation destruction dynamique des symboles)
    ;   Manipulation d'atomes (symbole ou numérique) et de liste (sous forme de listes chaînées)
    ;   Boucle d'interaction sur les expressions (lecture, evaluation puis impression)
    
;; Exercice 2 : Système expert

; 4) Ecrivez une fonction regles-candidates (But Regles) renvoyant
;la liste des règles candidates pour prouver le but But parmi les
; règles de la liste Regles.

(defun regles-candidates (But Regles)
  (let (res)
    (dolist (x Regles res)
      (if (eq but (cadr x))
        (push x res)
      )
    )
  )
)

; 5) Proposez un algorithme (clairement présenté et écrit en français) effectuant le parcours de
; l’arbre que vous avez tracé à la question 3. Vous utiliserez pour cela la fonction regles-
; candidates et les fonctions de service suivantes :
; • premisses (R) : permet d’obtenir les premisses d’une règle R
; • conclusion (R): permet d’obtenir la conclusion de la règle R
; • vrai? (fait) : permet de savoir si fait est dans la base de faits (avec la valeur vrai).

; Vérifier (but)
; -------------------
;   OK = FAUX
;   Si (vrai? BUT)
;     Retourner VRAi
;   Sinon
;     EC = candidates(but)
;     Pour chaque règle candidates de EC et tant que (non OK)
;       OK = verifierRegle(r)
;     FinPour
;     Retourner OK
;   FinSi
; -------------------
;
; =================================
;
; VérifierRegle (R)
; -------------------
;   OK = VRAi
;   PREM = premisses(R)
;   Pour chaque prémisse p de PREM et tant que OK
;     OK = verifier(p)
;   FinPour
;   Retourner OK
; -------------------



;; Exercice 3 : Programmation Lisp


(defparameter *arbre* '(
    (Lea Julien Laure)
    (Julien Pierre Margot)
    (Pierre Jacques Julie)
    (Margot Jean Maud)
    (Laure Paul Aline)
    (Paul Georges Rose)
    (Aline Dominiques Chaudine)
    )
)

; 1) Ecrivez deux fonctions Pere et Mere qui renvoient respectivement le nom du père ou de la
; mère d’une personne passée en paramètre.

(defun pere (pers)
  (cadr (assoc pers *arbre*))
)

(defun mere (pers)
  (caddr (assoc pers *arbre*))
)


; 2) Ecrivez une fonction GrandsPeres qui renvoie une liste composée des deux grands pères.

(defun grandsPeres (pers)
  (cons (pere (pere pers)) (pere (mere pers)))
)

;3) Généralisez cette fonction afin d’obtenir une fonction Ancetre qui renvoie une liste
; comprenant tous les ancêtres masculins d’une personne (ne considérez que les liens
; paternels).

(defun ancetre (pers)
  (if (pere pers)
    (cons (pere pers) (ancetre (pere pers)))
  )
)


; 4) Ecrivez une fonction Enfant qui renvoie l’enfant d’une personne.

(defun enfant (pers)
  (let (res)
    (dolist (x *arbre* res)
      (if (or (eq (mere (car x)) pers) (eq (pere (car x)) pers))
        (setq res (car x))
      )
    )
  )
)

; 5) Ecrivez une fonction Descendant? qui renvoie vrai si le premier paramètre est un
; descendant du second.

(defun descendant? (pers1 pers2)
  (if pers2
    (if (eq pers1 (enfant pers2)) ; si pers1 est l'enfant de pers2
      T
      (descendant? pers1 (enfant pers2)) ; sinon on regarde si c'est l'enfant de son enfant
    )
    nil
  )
)


;; Ou bien en s'appuyant sur la fonction des ancêtres (si pers 2 est ancetre de pers1 alors pers1 est descendant de pers2)

(defun descendant?? (pers1 pers2)
(not (eq (member pers2 (ancetre pers1)) nil))
)



;; Exercice 4 Parcours de chemin d’un Personnage Non Joueur (PNJ)


; 1) Définition formelle du problème

; Ensemble état accessibles : {1,...,26} U {E,S}
; Ensemble I des états initiaux : {E}
(defparameter *I* '( E ))
; Ensemble F des états finaux : {S}
(defparameter *F* '( S ))
; Ensemble des actions : {D,G,H,B}
;
; 2) Proposez une fonction de service « perception » qui construit les successeurs de l’état
; courant en demandant à l’utilisateur de rentrer les états suivants selon le formalisme proposé.
; Cette perception est alors indépendante des capacités du PNJ. La fonction « perception »
; ajoutera cette liste à la représentation du PNJ.
; Rappel : vous utiliserez la fonction (read) pour saisir la perception.

(defun perception (num)
  (let ((presence (assoc num *graph*)))
    (format t "Vous êtes dans l'état ~s, entrez les états possibles perçus par le PNJ (sous forme de a-list) : ~%" num)
    (if presence
      (defparameter *graph* (remove presence *graph*))
      )
    (push (read) *graph*)
  )
)

(defun initialiserGraph (choix)
; choix 0 : reprend tout, choix = 1 : un seulement
  (if (= choix 0)
    (progn
      (defparameter *graph* nil)
      (dotimes (x 28)
        (cond
          ((= x 0) (perception 'E))
          ((= x 27) (perception 'S))
          (t (perception x))
        )
      )
    )
    (progn
      (format t "Quel etat ? -> ")
      (perception (read))
    )
  )
)

(defun successeurs-valides (E V C)
; E : etat courant ; V : etats visités ; C : capacités du PNJ
  (let (suc)
    (if (= C 0)
      ; si le PNJ n'est pas sportif
      (dolist (x (cdr (assoc E *graph*))) ; on parcourt les états successeurs
        (if (and (not (eq (cdr x) 'obstacle)) (not (member (car x) v))) ; on regarde si ce n'est pas un obstacle où s'il n'a pas déjà été visité
          (push x suc) ;si c'est le cas on le rajoute aux successeurs
        )
      )
      ; s'il est sportif
      (dolist (x (cdr (assoc E *graph*))) ; on parcourt les états successeurs
        (if (not (member (car x) v)) ; on regarde si ce n'est pas déjà visité
          (push x suc) ;on le rajoute aux successeurs
        )
      )
    )
    suc ; on renvoit suc
  )
)

;4) Donnez les grandes lignes de l’algorithme qui permettrait au PNJ d’explorer
; l’environnement en entrant par E et cherchant la sortie S. Pensez à appeler les fonctions de
; services proposées dans les questions 3 et 4.

; On fait un algo pour explorer en profondeur (simule au mieux le déplacement)
;
; Profondeur (etat etatsVisites)
;   Si état fait partie des états finaux
;     On retourne le chemin (concaténation etat etatsVisités)
;   Sinon
;     Pour chaque successeurs s valide de etat
;       On parcourt en profondeur s [profondeur s (etat etatsVisites)]


(defun explore (E V C)
  (if (eq E 'S)
    (print (reverse (cons E V)))
    (let ((listSucc (successeurs-valides E V C)))
      (dolist (s listSucc)
        (explore s (cons E V) C)
      )
    )
  )
)
