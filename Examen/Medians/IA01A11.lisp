;; Médian IA01 A11

;; Exercice 1 :

; 1) A quoi sert la représentation des connaissances ?
; Elle sert à donner à des connaissances implicites d'un domaine particulier en les explicitant .
; Elle permet de mettre au point des systèmes intelligents qui peuvent manipuler ces connaissances
; dans le but de résoudre des problèmes mais aussi d'améliorer celles-ci.
; En ce sens, la représentation des connaissances constitue le support initial que l'on met en place sur ces connaissances
; pour les améliorer.

; 2) Donnez les différentes approches de l’Intelligence Artificielle.

; Approche logique : analyse du raisonnement - système experts
; Approche numérique/apprentissage : dévellopement d'algo qui utilisent des méthodes d'apprentissage
; et qui peuvent apprendre à faire une tâche grâce à un entrainement - machine learning
; Approche hybride
; Approche collective/distribuée - système multi-agents

; 3) Donnez les composants d’un système expert et précisez leur rôle et/ou caractéristiques.
; Base de faits : sorte de mémoire à court terme, socle sur lequel le travail ce fait (symptômes d'un patient)
; Base de règles : sorte de mémoire à long terme, socle de connaissances expertes sur un sujet (origine des causes des symptômes, règles générales ...)
; Moteur d'inférence : celui qui fait le lien entre les deux : regarde les faits et les confrontent à des règles (chainage avant)
; ou parcourt les règles et analyse pour voir si certains faits ne peuvent pas les déclencher (chainage arrière)

; 4) Qu’est-ce qu’un réseau sémantique ? Bien que très intéressant, il présente des
; problèmes de représentation. Donnez deux de ces problèmes.
; Un réseau sémantique est un type de représentation des connaissances.
; Ils sont nés des travaux de Quillian en linguistique et permettent de faire le lien entre des
; concepts. Ils ont ensuite été repris par Falhman en intelligence artificielle.
; 1er problème : pas de quantification universelles : Impossible de dire que "tous les plats confectionnés par Julien sont délicieux"
; 2eme problème : confusion entre les instances de concepts et les concepts eux-mêmes
; "Ils existent de nombreux fruits" : en quantité (10000 bananes ?) ou en concepts (10000 variétés ?)

; 4) Qu’est-ce qu’un frame ? Quelles relations entretient-il avec un réseau sémantique ?
; Chepa.

; 5) Donner les trois principales caractéristiques du langage Lisp.
; 1. Programmation fonctionnelle ; un programme est répartie sur plusieurs
; fonctions qui s'appellent entre elles. Cela permet une meilleure flexibilité.
; 2. Programmation symbolique : on manipule des symboles tout le temps, les valeurs sont elles même
; des symboles. Ceux-ci existent sur le moment et son remplacer leur de l'execution.
; 3. Programmation interactive : on peut directement avoir un resutlat depuis l'interateur de commande
; LISP se rapproche des langages interprétés comme Python.

; 6) Qu’est-ce qu’une paire pointée ? Donnez sa structure interne, précisez comment elle
; peut être prise en compte pour représenter la liste '(a b c).
;
; C'est un paire pour laquelle chacun des éléments pointent sur une variable.
; Normalement c'est une structure de liste chainée qui est utilisée.
; On peut faire : '(a . (b . c)) pour représenter '(a b c) sous forme de liste pointée.

;; Exercice 2 : Recherche dans un espace d’états

;; Représentation des opérateurs : on représente ce que l'on peut faire dans le cas où la case est vide

(defparameter *operators* '(
    (0 (1 0) (3 0))
    (1 (0 1) (2 1) (4 1))
    (2 (1 2) (5 2))
    (3 (0 3) (4 3) (6 3))
    (4 (1 4) (3 4) (5 4) (7 4))
    (5 (2 5) (4 5) (8 5))
    (6 (3 6) (7 6))
    (7 (4 7) (6 7) (8 7))
    (8 (5 8) (7 8))
  )
)

(defun valid-operators (state)
  (cdr (assoc (position '* state) *operators*))
)

(defun exchange (i j state)
  (let ((vali (nth i state)) (valj (nth j state)) res)
    (dolist (x state (reverse res))
      (cond
        ((eq x vali) (push valj res))
        ((eq x valj) (push vali res))
        (t (push x res))
      )
    )
  )
)

(defun apply-operator (op state)
  (let ((i (car op)) (j (cadr op)))
    (exchange i j state)
  )
)

(setq *etatFinaux* '((A B C D E F G H *)))

(defun profondeur (E V)
  (if (member E *etatFinaux* :test 'equal)
    (print (reverse (push E V)))
    (let ((OP (valid-operators E)))
      (dolist (o OP) ; il faut utiliser une fonction de type successeurs ici, sinon cela boucle à l'infini
        (profondeur (apply-operator o E) (cons E V))
      )
    )
  )
)

;; Exercice 3 : les Sims à l'UTC

(defvar SimsUtceens '(
  ((nom IsaacAsimoc) (Humeur 15.0) (Personnalite (nevrotisme 0.1) (extraversion 0.5)))
  ((Humeur -5.0) (nom FrankHerbert) (Personnalite (nevrotisme 0.5) (extraversion 0.1)))
  ((Personnalite (nevrotisme 0.5) (extraversion 0.5)) (nom KDick) (Humeur 10.0))
  )
)

; Définir la fonction lisp f1 qui affiche tous les noms de la liste de Sims passée en
; argument (0,5 points)
(defun f1 (liste)
  (let (res)
    (dolist (x liste res)
      (push (cadr(assoc 'nom x)) res)
    )
  )
)

; Définir la fonction lisp f2 qui renvoie la liste des caractéristiques pour un nom passé en
; argument. (1,5 point)

(defun f2 (nom liste)
  (let (res)
    (dolist (x liste res)
      (if (eq (cadr (assoc 'nom x)) nom)
        (setq res x)
      )
    )
  )
)

; Définir la fonction lisp f3 qui renvoie la liste des personnes dont l’humeur est supérieure
; à la valeur passée en argument. (1 point)

(defun humeur (x)
  ;fonction outil
  (cadr (assoc 'humeur x))
)

(defun f3 (seuil liste)
  (let (res)
    (dolist (x liste res)
      (if (> (humeur x) seuil)
        (push x res)
      )
    )
  )
)

; Définir les fonctions vExtraversion et vNevrotisme qui renvoient respectivement la
; valeur d’extraversion et de névrotisme pour une liste de caractéristiques d’une personne.
; (1,5 point)

(defun vExtraversion (x)
  (cadr (assoc 'extraversion (cdr (assoc 'Personnalite x)))) ; il faut aller récupérer la sous-liste Personnalite
)

(defun vNevrotisme (x)
  (cadr (assoc 'nevrotisme (cdr (assoc 'Personnalite x)))); il faut aller récupérer la sous-liste Personnalite
)


; Définir la fonction lisp f4 qui met à jour la valeur de l’humeur de chaque Sims lors d’un
; événement evt passé en argument (valeur positive pour un événement ressenti comme un
; événement positif et inversement).
; Pour chaque Sims, on propose la formule suivante :
; SI evt > 0 ALORS nouvelleHumeur <-ancienneHumeur + (evt * vExtraversion)
; SINON nouvelleHumeur <-ancienneHumeur + (evt * vNevrotisme)


(defun f4 (evt liste)
  (dolist (x liste liste)
    (if (> evt 0)
      (setf (cadr (assoc 'humeur x)) (+ (humeur x) (* evt (vExtraversion x))))
      (setf (cadr (assoc 'humeur x)) (+ (humeur x) (* evt (vNevrotisme x))))
    )
  )
)
