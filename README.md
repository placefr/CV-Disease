# CV-Disease
## CVD : Cardiovascular Disease Prediction

This study is an original work based on the "Cardiovascular Disease dataset", 
available on Kaggle, with this link : 
https://www.kaggle.com/sulianova/cardiovascular-disease-dataset.
The owner of the dataset is Svetlana Ulianova.

The goal of our project is to predict presence or absence of a cardiovascular disease.
The dataset provides several features in the medical field for every patient (factual information,
results of examination and information given by the patient), and the target variable (cardio).
The value of cardio is zero or one. One represents a patient with a cardiovascular problem.

The description of the features are here :

+ Age | Objective Feature | age | int (days)
+ Height | Objective Feature | height | int (cm) |
+ Weight | Objective Feature | weight | float (kg) |
+ Gender | Objective Feature | gender | categorical code | 1=woman 2=man
+ Systolic blood pressure | Examination Feature | ap_hi | int |
+ Diastolic blood pressure | Examination Feature | ap_lo | int |
+ Cholesterol | Examination Feature | cholesterol | 1: normal, 2: above normal, 3: well above normal
+ Glucose | Examination Feature | gluc | 1: normal, 2: above normal, 3: well above normal
+ Smoking | Subjective Feature | smoke | binary | 1=yes
+ Alcohol intake | Subjective Feature | alco | binary | 1=yes
+ Physical activity | Subjective Feature | active | binary | 1=yes
+ TARGET : Presence or absence of cardiovascular disease | cardio | binary | 1=yes


There are six main parts in the report :

+ data cleaning and preparation

+ dataset : general information and figures

+ dataset : general information - graphs

+ analysis section 1 : potential predictors

+ analysis section 2 : performance of several models

+ result section
