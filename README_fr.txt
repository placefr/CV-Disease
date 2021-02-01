
## CVD : Prédiction de maladie cardiaque

Cette étude est un travail original basé sur le dataset "Cardiovascular Disease
dataset", disponible sur Kaggle, avec ce lien :
https://www.kaggle.com/sulianova/cardiovascular-disease-dataset
L'auteur / propriétaire du dataset se nomme Svetlana Ulianova.

L'objectif du projet est de prédire la présence ou l'absence de maladie
cardiovasculaire. Le dataset fournit plusieurs informations dans le champ
médical pour chaque patient (information factuelle, résultat d'examen et
informations données par le patient), ainsi que la variable cible (cardio).
La valeur de cardio est 0 ou 1. 1 représente un patient avec un problème
cardiovasculaire.

La description des prédicteurs est reprise ci-dessous : 

+ Age | info factuelle | age | int (jours)
+ Taille | info factuelle | height | int (cm)
+ Poids | info factuelle | weight | float (kg)
+ Genre | info factuelle | gender | categorical code | 1=féminin 2=masculin
+ Pression systolique | résultat d'examen | ap_hi | int
+ Pression diastolique | résultat d'examen | ap_lo | int
+ Cholesterol | résultat d'examen | cholesterol | 1: normal, 2: au-dessus de la normale, 3: très au-dessus de la normale
+ Glucose | résultat d'examen | gluc | 1: normal, 2: au-dessus de la normale, 3: très au-dessus de la normale
+ Fumeur | info patient | smoke | binary | 1=oui
+ Consommation d'alcool | info patient | alco | binary | 1=oui
+ Activité physique | info patient | active | binary | 1=oui
+ Variable cible : Présence ou absence de maladie cardiovasculaire | cardio | binary | 1=oui


Le rapport se décompose en six parties :

+ préparation et nettoyage des données

+ dataset : informations générales et chiffres de synthèse

+ dataset : informations générales présentées graphiquement

+ analyse section 1 : prédicteurs potentiels

+ analyse section 2 : performance comparée de plusieurs modèles

+ résultats

