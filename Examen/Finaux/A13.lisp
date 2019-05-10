#| IA01 - A13 | 14h55

I Qu'avez-vous retenu du cours ?

1. Positionner la notion d'ontologie vis-à-vis des travau sur les logiques de descriptions,
les frames et les classes des langages de représentation par objets.

  Les ontologies sont l'aboutissement des travaux sur la représentation des connaissances
qu'il y a pu avoir précédemment avec les précédents travaux dans ce domaine que sont
les logiques de descriptions, les frames et les classes de langages des langages de représentation
par objet. Les ontologies définissent des terminologies que différents acteurs et logiciels
doivent utiliser pour collaborer dans un même domaine, de plus, si elle est basée sur une
ou des taxinomies de termes, elle ne s'y limite pas : elle définit aussi des relations
qu'entretiennent les différents concepts.

2. Quelles sont les trois principaux opérateurs utilisés dans une mise en oeuvre d'un
algorithme génétique ? Donnez l'algorithme simplifié, utilisant ces trois opérateurs,
explicitant la mise en oeuvre.

  Les trois principaux opérateurs utilisés sont : la sélection, l'hybridation et la mutation.

  Algo :
  T = 0
  P = population initiale
  Tant que (non critereDArrêt) faire :
    S = selection(P)
    H = Hybridation(S)
    P = Mutation(S)
    T ++

3. Quelles sont les quatre caractéristiques à préciser qui permettent de définir un réseau
de neurones artificiels ?

  Le type de neurones
  La topologie du réseau
  La dynamique du réseau
  Le type d'apprentissage


4. Donner quatre propriétés remarquables des RNA.

  La résistance au bruit
  La capacité de généralisation à partir d'apprentissage
  L'adaptabilité (apprentissage en continu)
  La distribution de la mémoire

5. Que fait la fonction ci-dessous F1 ? A quelle fonction standard lisp correspond-elle ?

  (defun F1 (L)
    (cond
    ((null L) 0)
    ((+1 (F1 (cdr L))))))

  Cette fonction renvoit 0 si la vide est vide ; sinon elle renvoit 1 plus le nombre d'élements
  qui restent dans la liste. Ainsi elle retourne le nombre d'éléments de la liste cad sa longueur :
  Elle correspond à la primitive list-length


II. Frames

  On ne considère ici que les facettes : $value, $default, $if-needed, $if-added, $if-removed

  Deux types de frames : ceux des concepts et ceux des individus.

  "On pourra toutefois considérer que l'indentifiant d'un concept est le symbole qui le nomme".

  |#

  (setq Artwork '(Artwork
                    (type ($value concept))
                    (number ($default 0))
                    (title ($if-nedded create-title))
                    (location ($default "Musée d'Orsay"))
                    (date ($default "Unknown"))))

  (setq Painting '(Painting
                    (is-a ($value Artwork))
                    (author ($default "Paul Gauguin"))
                    (style ($default "Impressionniste") ($id-added check-with-date))
                    (theme ($if-needed find-it-in-db))))

  #|

  1. Création des individus

  > (make-individual 'Painting 'author "Paul Gauguin" 'date 1875 'title "La seine au pont d'Iéna")
  T35

  1.1 Ecrire le frame associé à ce tableau d'identifiant T35

  (T35  (is-a ($value Painting))
        (author ($value "Paul Gauguin"))
        (date ($value 1975))
        (title ($value "La seine au pont d'Iéna")))

  1.2 Comment peut-on savoir qu'il s'agit d'un tableau de sytle "Impressionniste" ?

    Si le style n'est pas renseigné il est pas défaut sur cette valeur, c'est le cas ici.

  1.3 Oui.

  1.4
    Algorithme pour la fonction make-individual(concept listProp) :
      Données :
        concept : le concept auquel se rattache l'objet
        listProp : la liste des slots suivis des valeurs (en lisp on utilisera &rest)
      Début
        Créer un identifiant pour le frame (en lisp on utilisera (gentemp "T"))
        slot  = (pop listProp)
        attrib = (pop listProp)
        Si le slot est présent dans les frames parent (concept et au dessus) avec allowed-slot
          Si le slot présent des facettes procédurales les exécuter
          Ajouter au frame la liste (slot (value attrib))

        Ajouter en début du frame son identifiant
        Ajouter le frame aux frames globaux
      Fin

  1.5 Écrire la fonction allowed-slots |#

  (defun allowed-slots (frame slot)
    (if (or (null frame) (null slot))
      nil
      (or (assoc slot (cdr frame))
          (allowed-slots (symbol-value (cadadr (assoc 'is-a (cdr frame)))) slot))))

  #| 1.6 Écrire la fonction make-individual |#

(defparameter *frames* nil)

  (defun make-individual (concept &rest listProp)
    (cond
      ((not (member concept *frames*))
        (format t "~a n'est pas défini" concept))
      ((null listProp)
        (format t "Pas de propriétés passés"))
      (t
        (let (new (id (gentemp "T")) attrib slot champ)
          (while listProp
            (setq slot (pop listProp))
            (setq attrib (pop listProp))
            (setq champ (allowed-slots (symbol-value concept) slot))
            (cond
              ((not champ)
                (format t "Concept ~a non défini"))
              ((assoc '$if-needed (cdr champ))
                (funcall (cadr (assoc if-needed (cdr champ))) slot attrib))
              (t(push `(,slot ($value ,attrib)) new))))
          (push id new)
          (set id new)
          (push id *frames*)))))

#|
  2. Accès à l'information

  2.1 On souhaite disposer d'une fonction getv permettant d'obtenir la valeur d'une propriété
  pour un individu

  Décrire le fonctionnemetn de getv pour chaun de ces exemples.

  Pour les deux premiers il se trouve dans le slot dans T35 pour le dernier c'est dans le frame
  du concept.

  2.2 Ecrire l'algorithme de la fonction getv permettant d'obtenir la valeur d'une propriété pour un
  individu.

  Algo pour getv

    Données :
      oframe : le frame de l'individu, frame initiale
      frame : le frame actuel dans lequel on cherche
      slot : le slot que l'on chercher
    Début :
    Si le frame n'est pas null :
      Si le slot est présent dans le frame
        s'il y a une facette procédurale $if-needed
          on exécute le deamon associé et on retourne sa valeur
        Sinon
          on retourne la valeur indiquée par la facette $valeur
      Sinon on Cherche dans le frame père
    Sinon
      on renvoit nil

 3. Recherche
 |#

 (defun findArtworks (&key author debut fin)
  (if (> debut fin)
    (error "Debut > fin")
    (let (dateTemp)
      (dolist (f *frames*)
        (setq dateTemp (getv f 'date))
        (when (and (eq (getv f 'author) author) (>= dateTemp debut) (<= dateTemp fin))
          (format t "~A : ~A " (getv f 'titre) dateTemp))))))
#|

III. Raisonnement à partir de cas

1.
( (Titre . "Alien I")
  (Réalisateur . "Georges Lucas")
  (Annee . 1977) (Style . (ScienceFiction SpaceOpera))
  (Acteurs . ("Mark Hamill" "Harrison Ford"))
  (Note . 4))

2. Le raisonnement à partir de cas à pour but de résoudre des problèmes proches de problèmes
déjà rencontrés et résolus : il se base sur l'expérience de situtations déja vécues.

Il fonctionne en 3 étapes :
  - La remémorisation : on trouve le problème déjà vécu le plus semblable au problème que l'on rencontre
  - L'adaptation : on adapte les solutions du problème déjà vécu à ce nouveau problème
  - La mémorisation : on mémorise ce dernier pour plus tard.

3. Mesure de similarité :
    Si acteur présent dans les deux films : +10 points de similarité
    Si même réalisateur : + 30 points
    Si même genre : +10 poitns de similirité
    Si plus de 50 ans d'écart pas de points
      Si proche écart de x années : 50 - x points de similarité sinon
 |#

 (defun similarite (c1 c2)
  (let (score ecart)
    (if (eq (getv c1 'realisateur) (getv c2 'realisateur))
      (setq score (+ score 30)))
    (setq score (+ score (* 10 (list-length (intersection (getv c1 'acteurs) (getc c2 'acteurs))))))
    (setq score (+ score (* 10 (list-length (intersection (getv c1 'style) (getc c2 'style))))))
    (setq ecart (abs (- (getv c1 'annee) (getv c1 'annee))))
    (if (<= 50 ecart)
      (setq score (+ score (- 50 ecart))))
    score))
 #|
4. Algorithme pour CasLePlusProche(casCible,MaBaseCas)
    Début :
      ListeSimilarité : contiendra des couple (TitreFilm . similarite)
      Pour chaque cas c de MaBaseCas:
        Ajouter à ListeSimilarité (TitreFilm(c) . similarite(c,casCible))

      Retourner le cas de similarite maximal

5.
