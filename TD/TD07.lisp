;; TD07

;;1. Simulation de marquage

;; On propage M1 le long des is-a en sens inverse
;;les nobles aimés de Roxane sont les destinations des arcs "aime" dont l'origine est marquée par M2 et 
;;l'extreté pas M1


; - On marque respectivement les nœuds Noble et Roxane par M1 et M2.
; - On propage en parallèle la marque M1 le long des arcs "is-a" dans le sens contraire.
; - On interroger les liens "Aime" marqués à gauche par M2 et à droite par M1.
; La réponse est : Christian (marqué par M1) : Roxanne aime Christian


;; pour marquer in noeud on ajoute la marque à une properté "mark" de la a-list


;;2. Implémentation sous LISP
(defun mark_node (id mark)
    (add_prop_val id 'mark mark)
)

(defun get_marked_nodes (mark)
  ; pour obtenir les noeuds marqué selon mark dans le réseau
    (let ((res))
        (dolist (x *noeuds* (reverse res))
            (if (eq (get 'mark x) mark)
                (push x res)
            )
        )       
    )   
)

;;---------------------version  Prof
;; mark_nodes <- get_marked_nodes(mark)
;; tant que mark_nodes !=nil
;;     new_marked_nodes <- nil 
;;     pour chaque noeud de marked_nodes
;;         si direction = direct   
;;             next-noeude <-success (noeud type-arc)
;;         sinon
;;             nex-noeuds <- predecesses (noeud type-arc)
;;         finsi
;;         si next-noeuds != nil
;;             pour chaque n de next-noeuds
;;                 si n non marqué par mark
;;                     marquer n
;;                     new_marked_nodes <- new_marked_nodes + n
;;                 finsi
;;             finpour
;;         finsi
;;     finpour
;;     mark_nodes <-new_marked_nodes
;; fintantque

;;function : marked?(node mark)     successors(node type-arc)       pred(node type-arc)

(defun wave (mark prop sens)
  ; pour propager les marques
  (if (or (eq sens 'in) (eq sens 'out))
    (let (
        (marked-nodes (get-marked-nodes mark)) ; noeud déjà marqués
        next-node ; prochains noeuds
        new-marked-node) ; noeuds à marquer
        (loop
            (setq new-marked-node nil) ;
            (dolist (node marked-nodes) ; pour chaque noeuds marqués
              (setq next-node
                (if (eq sens 'in)
                  (succ node prop) ; on récupère ces successeurs
                  (pred node prop))) ; on récupère ces prédecesseurs
              (when next-node ; si la liste n'est pas vide
                  (dolist (x next-node)
                    (unless (marked? x mark)
                      (mark-node x mark)
                      (pushnew x new-marked-node))))
            )
            (format t "~& ~A -> ~A" marked-nodes new-marked-node)
            (unless new-marked-node ; s'il n'y a pas de nouveaux noeuds à traiter : on arrête
              (return-from wave t))
            (setq marked-nodes new-marked-node)
        )
    ) ; on attribue les nouveaux noeuds
    (format t "~& Erreur : sens mal entré : ~A" sens))
)

(defun marked? (node mark)
  ; pour savoir si marqué selon mark
  (eq (get 'marque node) mark))
(defun pred (node prop)
  ; pour obtenir les prédecesseurs
  (let (res)
    ; on parcourt la liste des arcs x entrant
        (dolist (x (cdr (assoc 'INARCS (symbol-value node))) (reverse res))
        ; si l'arc x a est du type prop recherché
        (if (eq (cdr (assoc 'TYPE (symbol-value x))) prop)
            ; on retient le nœud d'origine de l'arc (un prédecesseur)
            (push (cdr (assoc 'FROM (symbol-value x))) res)
            )
        )
    )
)

(defun succ (node prop)
  ; pour obtenir les successeurs
  (let (res)
    ; on parcourt la liste des arcs x entrant
    (dolist (x (cdr (assoc 'OUTARCS (symbol-value node))) (reverse res))
      ; si l'arc x a est du type prop recherché
      (if (eq (cdr (assoc 'TYPE (symbol-value x))) prop)
        ; on ajoute le nœud destination de l'arc (un successeur)
        (push (cdr (assoc 'TO (symbol-value x))) res)
      )
    )
  )
)

(defun get-results (markleft markright type)
    (dolist (x (symbol-value type))
    ; (print x)
    ; (print (cdr (assoc 'from-node (symbol-value x))))
    ; (print (cdr (assoc 'to-node (symbol-value x))))
        (when (AND
                (eq (get 'marque (cdr (assoc 'from-node (symbol-value x)))) markleft)
                (eq (get 'marque (cdr (assoc 'to-node (symbol-value x)))) markright)
              )
            (format t "~S ~S ~S~&"
                (cdr (assoc 'name (symbol-value (cdr (assoc 'from-node (symbol-value x))))))
                type
                (cdr (assoc 'name (symbol-value (cdr (assoc 'to-node (symbol-value x))))))
            )
        )
    )
)