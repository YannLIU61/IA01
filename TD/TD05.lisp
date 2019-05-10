(setq *BR* '(
                (R1 (B D E) F)
                (R2 (D G) A)
                (R3 (C F) A)
                (R4 C D)
                (R5 D E)
                (R6 A H)
                (R7 B X)
                (R8 (X C) A) )
)
(setq *BF* '(B C))

;; fonction des services
;; CCL Regle(Regle)
;; prenmisse Regle()
;; numRegle()

(defun CCLRegle (regle)
    (caddr regle)
)
(defun premisseRegle (regle)
    (cadr regle)
)
(defun nunmRegle(regle)
    (car regle)
)

(defun ReglesCandidates(But bdR)
    (let ( ( candidates '() ) ) 
        (loop for x in bdR
                do (if (equal But (CCLRegle x) ) 
                        (push  x candidates)
                        nil
                    )
            )
        (print candidates)
     )
    ;;     (dolist (x bdR candidates)
    ;;         (if (equal But (CCLRegle x))
    ;;         (push x candidates))
    ;;     )
    ;;     (print candidates)
    ;; )   
)

;; (ReglesCandidates 'A *BR*)
;; ((R8 (X C) A) (R3 (C F) A) (R2 (D G) A)) 

(defun ReglesCandidatesR (But bdR)
    (if bdR
        (if (equal But (CCLRegle (car bdR)))
            (cons (car bdR) (ReglesCandidatesR But (cdr bdR)))
            (ReglesCandidatesR But (cdr bdR))
        
        )
    )
)

;; (defun Chainagearr (But bdR)
;;     (let (( candidates (ReglesCandidates But bdR) ))
;;         (dolist (x candidates nil)

;;         )
  
;;     )
;; )

(defun verifier_ou (But bdR bdF)
    ;; version1 
    ;; si But dans bdF 
    ;;     But prouve
    ;;     renvoyer true
    ;; sinon
    ;;     regles = reglescandidates (But bdR)
    ;;     pour chaque regle de regles  et tant que ok = nil
    ;;         on verifier que la premisse est prouve (ok = verifier-et ((pop regles) bdR bdF))
    ;;      envoyer ok

    ;; version2
    ;; si But dans bdF
    ;;     true
    ;; sinon
    ;;     (some #'(lambda (x)
    ;;     (verifier-et(premisseRegle x) bdR bdF))
    ;;     (ReglesCandidates But bdR))))
    (if (member But bdF)
        (setq OK t )
        (some #'(lambda (x) (verifier_et (premisseRegle x) bdR bdF)) (ReglesCandidates But bdR))
    )


)

(defun verifier_et (regle bdR bdF)
    ;; version1
    ;; premisse = premisseRegle(regle)
    ;; pour chaque element de premisse et tant que ok = true
    ;;    on verifier si l'element est prouve
    ;;     (ok = (verfier-ou (pop premisse) bdR bdF))
    ;;  revoyer ok

    ;; version2
    ;; (every #'(lambda(x)
    ;; (verfier-ou x bdR bdF))
    ;; regle))
    (every #'(lambda (x) (verifier_ou x bdR bdF)) regle)


)
(print(verifier_ou 'H *BR* *BF*))