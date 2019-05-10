;; (defclass $voiture()
;;     (
;;         ($marque :accessor ?marque  :initarg  :marque)
;;         ($model :accessor ?model  :initarg  :model)
;;         ($coulor :accessor ?coulor  :initarg  :coulor)
;;     )
;; )

;; (setq $1 (make-instance '$voiture :marque 'BMW :model 700 :coulor 'white))

;; (print (?marque $1))
;; (print (?coulor $1))

(defclass $voiture ()
  ((marque :accessor marque? :initarg :marque)
   (modele :accessor modele? :initarg :modele)
   (annee :accessor annee? :initarg :annee :type number)
   (km :accessor km? :initarg :km :type number)
   (etatV :accessor etatV? :initarg :etatV)
   (moteur :accessor moteur? :initarg :moteur :initform 'Diesel)
   (couleur :accessor couleur? :initarg :couleur :initform 'White)
   (prix :accessor prix? :initarg :prix :type number)))



; On définit ici les "classes" des voitures ; il y en a deux mais on pourrait en faire d'autre
(setq VoitureFamiliale (list
  (setq v1 (make-instance '$voiture :marque 'Citroen :modele 'C3 :annee 2010 :km 20000 :etatV 'Bon :prix 5000))
  (setq v2 (make-instance '$voiture :marque 'Peugeot :modele '206 :annee 2008 :km 100000 :etatV 'AssezBon :prix 3000))
  (setq v3 (make-instance '$voiture :marque 'Renault :modele 'Megane :annee 2012 :km 5000 :etatV 'Excellent :prix 10000))
  (setq v4 (make-instance '$voiture :marque 'Citron :modele 'Berlingo :annee 2000 :km 2000000 :etatV 'BienUtilise :prix 2000))))

(setq VoitureSport (list
  (setq v6 (make-instance '$voiture :marque 'Audi :modele 'TT :annee 2014 :km 2000 :etatV 'AssezBon :prix 30000))
  (setq v7 (make-instance '$voiture :marque 'RollsRoyce :modele 'RollsRoyce :annee 2012 :km 100000 :etatV 'Bon :prix 10000))
  (setq v8 (make-instance '$voiture :marque 'BatMobile :modele 'BatMobile :annee 2016 :km 0 :etatV 'Excellent :prix 1000000))
  (setq v9 (make-instance '$voiture :marque 'Tesla :modele 'Tesla :annee 2015 :km 200000 :etatV 'BienUtilise :prix 60000))))

(dolist x in VoitureSport
    (print (annee? x))


)