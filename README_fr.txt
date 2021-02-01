
## CVD : Pr�diction de maladie cardiaque

Cette �tude est un travail original bas� sur le dataset "Cardiovascular Disease
dataset", disponible sur Kaggle, avec ce lien :
https://www.kaggle.com/sulianova/cardiovascular-disease-dataset
L'auteur / propri�taire du dataset se nomme Svetlana Ulianova.

L'objectif du projet est de pr�dire la pr�sence ou l'absence de maladie
cardiovasculaire. Le dataset fournit plusieurs informations dans le champ
m�dical pour chaque patient (information factuelle, r�sultat d'examen et
informations donn�es par le patient), ainsi que la variable cible (cardio).
La valeur de cardio est 0 ou 1. 1 repr�sente un patient avec un probl�me
cardiovasculaire.

La description des pr�dicteurs est reprise ci-dessous : 

+ Age | info factuelle | age | int (jours)
+ Taille | info factuelle | height | int (cm)
+ Poids | info factuelle | weight | float (kg)
+ Genre | info factuelle | gender | categorical code | 1=f�minin 2=masculin
+ Pression systolique | r�sultat d'examen | ap_hi | int
+ Pression diastolique | r�sultat d'examen | ap_lo | int
+ Cholesterol | r�sultat d'examen | cholesterol | 1: normal, 2: au-dessus de la normale, 3: tr�s au-dessus de la normale
+ Glucose | r�sultat d'examen | gluc | 1: normal, 2: au-dessus de la normale, 3: tr�s au-dessus de la normale
+ Fumeur | info patient | smoke | binary | 1=oui
+ Consommation d'alcool | info patient | alco | binary | 1=oui
+ Activit� physique | info patient | active | binary | 1=oui
+ Variable cible : Pr�sence ou absence de maladie cardiovasculaire | cardio | binary | 1=oui


Le rapport se d�compose en six parties :

+ pr�paration et nettoyage des donn�es

+ dataset : informations g�n�rales et chiffres de synth�se

+ dataset : informations g�n�rales pr�sent�es graphiquement

+ analyse section 1 : pr�dicteurs potentiels

+ analyse section 2 : performance compar�e de plusieurs mod�les

+ r�sultats

