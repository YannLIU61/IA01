;; Médian IA01 A12

;; Exercice 1 :

; 1) A quoi sert la représentation des connaissances ?
; Elle sert à donner à des connaissances implicites d'un domaine particulier en les explicitant .
; Elle permet de mettre au point des systèmes intelligents qui peuvent manipuler ces connaissances
; dans le but de résoudre des problèmes mais aussi d'améliorer celles-ci.
; En ce sens, la représentation des connaissances constitue le support initial que l'on met en place sur ces connaissances
; pour les améliorer.

; 2) Qu’est-ce qu’une recherche dans un espace d’états ? Explicitez les conditions d’application.
; C'est un type de problème que l'on rencontre en intelligence artificielle.
; Il s'agit d'expliciter une situation (ou problème) sous forme de déplacement dans un espace d'états.
; Il faut que le problème soit bien formalisé ; on doit pouvoir : repérer les états initiaux et finaux ainsi que les règles
; permettant de passer d'un état à un autre

; 3) Qu’est-ce qu’un réseau sémantique ? Donnez une représentation de l’énoncé « L’étudiant
; Pierre aime le cours IA01 »
; Un réseau sémantique est un type de représentation des connaissances.
; Ils sont nés des travaux de Quillian en linguistique et permettent de faire le lien entre des
; concepts. Ils ont ensuite été repris par Falhman en intelligence artificielle.

; 4) Qu’est-ce qu’un frame ? Quelles relations entretient-il avec un réseau sémantique ?
; Chepa.

; 5) Donner les trois principales caractéristiques du langage Lisp.
; 1. Programmation fonctionnelle ; un programme est répartie sur plusieurs
; fonctions qui s'appellent entre elles. Cela permet une meilleure flexibilité.
; 2. Programmation symbolique : on manipule des symboles tout le temps, les valeurs sont elles même
; des symboles. Ceux-ci existent sur le moment et son remplacer leur de l'execution.
; 3. Programmation interactive : on peut directement avoir un resutlat depuis l'interateur de commande
; LISP se rapproche des langages interprétés comme Python.

;; Exercice 2 : Traitement de listes

; Je n'avais pas vu la note ensuite avec les mapcar, maplist et autre.
; J'ai donc fait ça de façon artisanale.

(defun substituer (sexpr alist)
  ;calcule la s-expression obtenue en remplaçant dans sexpr chaque
  ;symbole ayant une valeur dans alist
  (when sexpr
    (let (construction)
      (if (atom (car sexpr))
        (setq construction (cdr (assoc (car sexpr) alist)))
        (dolist (x (car sexpr))
          (setq construction (append construction (substituer (list x) alist)))
        )
      )
      (cons construction (substituer (cdr sexpr) alist))
    )
  )
)

(defun double-elements (liste)
  ;double les éléments (au premier niveau) d’une liste.
  (when liste
    (if (atom (car liste))
      (progn
        (cons (car liste) (cons (car liste) (double-elements (cdr liste))))
        )
      (cons (car liste) (double-elements (cdr liste)))
    )
  )
)

(defun double-map-elements (yolo)
  ;définition non récursive de double-elements
  (mapcan #'(lambda (x) (if (atom x) (list x x) (list x))) yolo)
)

(defun double-atomes (yolo)
  ;double les atomes d’une liste à tous les niveaux
  (when yolo
    (let (oktm)
      (if (atom (car yolo))
        (setq oktm (list (car yolo) (car yolo)))
        (dolist (lol (car yolo))
          (setq oktm (append oktm (double-atomes (list lol))))
        )
      )
      (cons oktm (double-atomes (cdr yolo)))
    )
  )
)



;; Exercice 3: Système Expert : StarshipTroopersExpert
;
;
; 1. Base de faits
; Eh bah on liste les faits les uns derrière les autres ; parcontre j'ai pas tellement compris l'énoncé
; donc j'ai pas trouvé de faits que l'on peut citer.

; 2. Règles
; J'ai du mal à voir comment on peut déduire certains résultats, on est forcé de faire de l'induction par moment.

; 4. Inférence :  Algorithme de chaînage-arrière profondeur d'abord :
;
; Vérifier (p)                                      || VériferRègle(r)
;   | OK = FAUX                                     ||   | OK = VRAI
;   | Si (p in BF)                                  ||   | PREM = Premisses(r)
;   |   |  Retourner(VRAI)                          ||   | Pour chaque p de PREM et tant que OK
;   | Sinon                                         ||   |   |   OK = Vérifier(p)
;   |   |  EC = ReglesCandidates(p)                 ||   | Retourner(OK)
;   |   |  Pour chaque r de EC et tant que (non OK) ||   |
;   |   |   |   OK = VérifierRègle(r)               ||   |
;   |   |  FinPour                                  ||   |
;   |   | Retourner(OK)                             ||
;   | FinSi                                         ||
;
;
; 5. Ordre 0+ :
; a. Il faudrait que l'on définisse des couples (attribut valeur) ou triplets (objet attribut valeur)
; au niveau des règles il faudrait travailler sur ces partie de couple ou triplets et nous pas directement sur les faits

; b. dans le moteur d'inférence, chepa.
