

# ------------------------------LIBRARIES
if (!require("readr")) {
  install.packages("readr")
  library(readr)
}

if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require("gridExtra")) {
  install.packages("gridExtra")
  library(gridExtra) #used for function grid.arrange
}

if (!require("ggthemes")) {
  install.packages("ggthemes")
  library(ggthemes) #used for theme_hc
}

if (!require("caret")) {
  install.packages("caret")
  library(caret)
}

if (!require("rpart")) {
  install.packages("rpart")
  library(rpart) #used for classification tree
}

if (!require("randomForest")) {
  install.packages("randomForest")
  library(randomForest)
}

options(digits=4, scipen=100) #force decimal notation
set.seed(10, sample.kind = "Rounding") #to fix the results of randomized processes


# ------------------------------LOAD DATAS (from placefr GitHub repository)
#url is : https://github.com/placefr/CV-Disease
filename= "cardio_train.csv"
path1 = "https://raw.githubusercontent.com/placefr/CV-Disease/master"
path2 <- getwd() #local path (path2) is set to working directory

p_f_remote <- file.path(path1, filename) #remote path and filename
p_f_local <- file.path(path2, filename) #local path and filename 

download.file(p_f_remote, filename)
dat <- read_csv2(p_f_local) #read the file, separator is semicolon
dat2 <- dat #dat2 is used in the following


# ------------------------------DATA CLEANING AND PREPARATION
#seeking for 'NA' in dat2 dataset
test1 <- ifelse(dat2 == "NA", 1, 0) 
#seeking for blank cells in dat2 dataset
test2 <- ifelse(dat2 == "" | dat2 == " ", 1, 0)
rtest1 <- sum(test1)
rtest2 <- sum(test2) #zero in both cases indicates no "NA" and no blank cells
#print results
rtest1
rtest2

#managing blood pressure out of range values
#ap_hi and ap_lo : a new column testbp is created and added to dat2
#with '2' if ap_hi or ap_lo are out of range, and '3' if values seems to be normal
testbp <- ifelse(dat2$ap_hi<40 | dat2$ap_hi>300 | dat2$ap_lo<5 | dat2$ap_lo>150, 2, 3)
dat3 <- data.frame(dat2,testbp)

#creating training set and test set
#the size of the test set is 10% of the global dataset
#we will use test set only during the final stage, to evaluate the performance
#of the selected model 
test_index <- createDataPartition(dat3$id, times=1, p=0.1, list=FALSE)
train <- dat3[-test_index,]
validation <- dat3[test_index,]

#training set preparation
out_of_range <- train %>% filter(testbp==2) %>% nrow()
normal <- train %>% filter(testbp==3) %>% nrow()
#print results
out_of_range
normal

train2 <- train %>% filter(testbp==3) #train2 is a subset of train,
#with less rows, and only "normal" values for blood pressure (ap_hi and ap_low)


# ------------------------------DATASET dat3 : GENERAL INFORMATION & FIGURES
#results are printed in the end of the section

#number of rows
row_nb <- nrow(dat3)

#number of women, number of men, and %, in the dataset dat2
wom_nb <- dat3 %>% filter(gender == 1) %>% nrow()
wom_percent <- wom_nb / row_nb * 100
men_nb <- dat3 %>% filter(gender == 2) %>% nrow()
men_percent <- men_nb / row_nb * 100

#women and men : average age & standard deviation
age_in_years <- dat3$age / 365 #age in years
dat4 <- data.frame(dat3,age_in_years)

avg_age <- dat4 %>% group_by(gender) %>% summarize(avg = mean(age_in_years))
sdev_age <- dat4 %>% group_by(gender) %>% summarize(sdev = sd(age_in_years)) #age : standard deviation

#women and men : average height & standard deviation
avg_height <- dat4 %>% group_by(gender) %>% summarize(avg = mean(height)/100)
sdev_height <- dat4 %>% group_by(gender) %>% summarize(sdev = sd(height)/100) #height : standard deviation

#women and men : average weight & standard deviation
avg_weight <- dat4 %>% group_by(gender) %>% summarize(avg = mean(weight)/10)
sdev_weight <- dat4 %>% group_by(gender) %>% summarize(sdev = sd(weight)/10) #weight : standard deviation

#print the first results
labels1 <- c("number of rows", "number of women", "% of women in the dataset", "number of men", "% of men in the dataset") 
results1 <- c(row_nb, wom_nb, wom_percent, men_nb, men_percent)
results_1 <- data.frame(labels1, results1)
results_1

#print results (following)
labels2 <- c("average age of women", "sd age of women", "average age of men", "sd age of men") 
results2 <- c(avg_age$avg[1], sdev_age$sdev[1], avg_age$avg[2], sdev_age$sdev[2])
results_2 <- data.frame(labels2, results2)
results_2

#print results (following)
labels3 <- c("average height of women", "sd height of women", "average height of men", "sd height of men") 
results3 <- c(avg_height$avg[1], sdev_height$sdev[1], avg_height$avg[2], sdev_height$sdev[2])
results_3 <- data.frame(labels3, results3)
results_3

#print results (following)
labels4 <- c("average weight of women", "sd weight of women", "average weight of men", "sd weight of men") 
results4 <- c(avg_weight$avg[1], sdev_weight$sdev[1], avg_weight$avg[2], sdev_weight$sdev[2])
results_4 <- data.frame(labels4, results4)
results_4


# ------------------------------DATASET dat2 : GENERAL INFORMATION / GRAPHS
dat5 <- dat4 %>% filter(testbp==3) #dat5 is clean (no out of range values for blood pressure)
avg1 <- mean(dat5$ap_hi) #avg1 is the average of ap_hi (men and women)
avg2 <- mean(dat5$ap_lo) #avg2 is the average of ap_lo (men and women)

#blood pressure high values, women
dat_wom <- dat5 %>% filter(gender==1)
nbw <- nrow(dat_wom) #number of women in dat5 dataset

index_w <- seq(1,nbw,1)
g1 <- ggplot(dat_wom, aes(x=index_w, y=ap_hi)) +
  ggtitle("Blood pressure high values, women") +
  geom_line(color="darkgrey") + 
  geom_hline(yintercept=avg1, color="red", size=1) #average line

#blood pressure high values, men  
dat_men <- dat5 %>% filter(gender==2)
nbm <- nrow(dat_men) #number of men in dat5 dataset

index_m <- seq(1,nbm,1)
g2 <- ggplot(dat_men, aes(x=index_m, y=ap_hi)) +
  ggtitle("Blood pressure high values, men") +
  geom_line(color="darkblue") + 
  geom_hline(yintercept=avg1, color="red", size=1) #average line

#blood pressure low values, women
g3 <- ggplot(dat_wom, aes(x=index_w, y=ap_lo)) +
  ggtitle("Blood pressure low values, women") +
  geom_line(color="lightgrey") + 
  geom_hline(yintercept=avg2, color="purple", size=1)

#blood pressure low values, men  
g4 <- ggplot(dat_men, aes(x=index_m, y=ap_lo)) +
  ggtitle("Blood pressure low values, men") +
  geom_line(color="lightblue") + 
  geom_hline(yintercept=avg2, color="purple", size=1) 

#print the four graphs
grid.arrange(g1, g2, ncol = 2)
grid.arrange(g3, g4, ncol = 2)

#score of risk
#we define a global score for predictors : cholesterol + gluc + smoke + alcohol
#it's the sum of the four individual indicators
#the sum represents a score of potential risk
#we print only for a subset of id [200:500,] in the dataset
score <- dat5$cholesterol + dat5$gluc + dat5$smoke + dat5$alco
dat6 <- data.frame(dat5,score)
dat6 <- dat6[200:500,]
g5 <- ggplot(dat6, aes(x=id, y=score, color=as.factor(score))) +
  scale_color_brewer(palette="Dark2") +
  theme_hc() +
  geom_point(size=1.8)
g5

#active & inactive people
#we plot the proportion of active women and men, by age
age_y_rounded <- round(age_in_years, digits = 0) #age in years, rounded
#this variable comes from section General Information / Figures
dat7 <- data.frame(dat4,age_y_rounded)

active_wom <- dat7 %>% filter(gender == 1 & active == 1) %>% 
  group_by(age_y_rounded) %>% summarize(num = n()) %>% mutate()

g6 <- active_wom %>% ggplot(aes(x=age_y_rounded, y=num, fill=factor(age_y_rounded))) +
  ggtitle("Number of active women, by age") +
  geom_col(width=0.8) + 
  scale_fill_grey() +
  theme(legend.position='none')

active_men <- dat7 %>% filter(gender == 2 & active == 1) %>% 
  group_by(age_y_rounded) %>% summarize(num = n()) %>% mutate()

g7 <- active_men %>% ggplot(aes(x=age_y_rounded, y=num, fill=factor(age_y_rounded))) +
  ggtitle("Number of active men, by age") +
  geom_col(width=0.8) + 
  scale_fill_grey() +
  theme(legend.position='none')

grid.arrange(g6, g7, ncol=2)


# ------------------------------ANALYSIS SECTION : POTENTIAL PREDICTORS
#based on train2 (the clean train set created above), we create a new training set and test set
#the new train set represents 50% of 'train2'
test_index <- createDataPartition(train2$id, times=1, p=0.5, list=FALSE)
trainset <- train2[-test_index,]
testset <- train2[test_index,]

str(trainset) #check structure of the new dataset
str(testset) 

trainset2 <- trainset #trainset2 is used in the following

#AGE / CARDIO
#graph : age vs cardio
#we seek if the trend seems to be linear
age_y_r <- round(trainset2$age/365)
trainset3 <- data.frame(trainset2,age_y_r)
trainset3 %>% 
  group_by(age_y_r) %>% filter(n() >= 20) %>% #we take groups of datas with more than 20 records
  summarize(prop = mean(cardio == 1)) %>%
  ggplot(aes(age_y_r, prop)) + 
  geom_point(size=0.8)
#looking for a link between age and cardio with lm()
fit <- lm(cardio ~ age, trainset2)
p_hat <- predict(fit, testset)
y_hat <- ifelse(p_hat > 0.5, 1, 0) %>% factor()
#accuracy
confusionMatrix(y_hat, factor(testset$cardio))$overall[["Accuracy"]]

#SEX / CARDIO
wom_prop <- trainset2 %>% filter(gender == 1) %>% nrow()
wom_cardio <- trainset2 %>% filter(gender == 1 & cardio ==1) %>% nrow()
wom_p <- wom_cardio / wom_prop

men_prop <- trainset2 %>% filter(gender == 2) %>% nrow()
men_cardio <- trainset2 %>% filter(gender == 2 & cardio ==1) %>% nrow()
men_p <- men_cardio / men_prop

options(digits=4)
#proportion of women with cardio=1 in trainset2
wom_p
#proportion of men with cardio=1 in trainset2
men_p

#WEIGHT / CARDIO
#graph : weight vs cardio
#we seek if the trend seems to be linear
trainset2 %>% 
  group_by(weight) %>% filter(n() >= 100) %>% 
  summarize(prop = mean(cardio == 1)) %>%
  ggplot(aes(weight, prop)) + 
  geom_point(size=0.8)
#looking for a link between weight and cardio with lm()
fit <- lm(cardio ~ weight, trainset2)
p_hat <- predict(fit, testset)
y_hat <- ifelse(p_hat > 0.5, 1, 0) %>% factor()
#accuracy
confusionMatrix(y_hat, factor(testset$cardio))$overall[["Accuracy"]]

#BLOOD PRESSURE / CARDIO
#graph : blood pressure vs cardio
#we seek if the trend seems to be linear
#graph : ap_hi
g10 <- trainset2 %>% 
  group_by(ap_hi) %>% filter(n() >= 20) %>% 
  summarize(prop = mean(cardio == 1)) %>%
  ggplot(aes(ap_hi, prop)) + 
  geom_point(size=0.8)
#graph : ap_lo
g11 <- trainset2 %>% 
  group_by(ap_lo) %>% filter(n() >= 20) %>% 
  summarize(prop = mean(cardio == 1)) %>%
  ggplot(aes(ap_lo, prop)) + 
  geom_point(size=0.8)

grid.arrange(g10, g11, ncol = 2)

#looking for a link between ap_hi and cardio with lm()
fit <- lm(cardio ~ ap_hi, trainset2)
p_hat <- predict(fit, testset)
y_hat <- ifelse(p_hat > 0.5, 1, 0) %>% factor()
#accuracy
confusionMatrix(y_hat, factor(testset$cardio))$overall[["Accuracy"]]

#looking for a link between ap_lo and cardio with lm()
fit <- lm(cardio ~ ap_lo, trainset2)
p_hat <- predict(fit, testset)
y_hat <- ifelse(p_hat > 0.5, 1, 0) %>% factor()
#accuracy
confusionMatrix(y_hat, factor(testset$cardio))$overall[["Accuracy"]]

#SCORE OF RISK (as defined above)
score <- trainset2$cholesterol + trainset2$gluc + trainset2$smoke + trainset2$alco
trainset4 <- data.frame(trainset2, score)
#graph : score vs cardio
g12 <- trainset4 %>% 
  group_by(score) %>% 
  summarize(prop = mean(cardio)) %>% #mean gives the proportion of people 
  #who are sick for each score of risk
  ggplot(aes(score, prop)) + 
  geom_point(size=1)

g12


# ------------------------------ANALYSIS SECTION : PERFORMANCE OF SEVERAL MODELS

# ----------MODEL 1 : LINEAR REGRESSION
#Predictors are :
#age, gender, weight, ap_hi and ap_lo, score of risk and physical activity
#our hypothesis is also that height is not linked to heart diseases in general

fit <- lm(cardio ~ age + gender + weight + ap_hi + ap_lo + active + score, trainset4)

#
score <- testset$cholesterol + testset$gluc + testset$smoke + testset$alco
testset_cpl <- data.frame(testset, score) #add score to clean version of testset

p_hat_model1 <- predict(fit, testset_cpl)
y_hat <- ifelse(p_hat_model1 > 0.5, 1, 0) %>% factor()
accur_lr <-confusionMatrix(y_hat, factor(testset$cardio))$overall[["Accuracy"]]


# ----------MODEL 2 : REGRESSION TREE
#a lightened version of trainset4 & testset_cpl are used
trainset_ctree <- trainset4 %>% select(id, age, gender, weight, ap_hi, ap_lo, active, cardio, score)
testset_ctree <- testset_cpl %>% select(id, age, gender, weight, ap_hi, ap_lo, active, cardio, score)

#Model 2.1 : works with standard parameters
fit <- rpart(cardio ~ .,
             data = trainset_ctree)
p_hat <- predict(fit, testset_ctree)
y_hat1 <- ifelse(p_hat > 0.5, 1, 0) %>% factor()
accur_rt1 <- confusionMatrix(y_hat1, factor(testset$cardio))$overall[["Accuracy"]]

#Model 2.2 : we try several values for complexity parameter
calc1 <- function(x){
  fit <- rpart(cardio ~ .,
               data = trainset_ctree,
               control=rpart.control(cp=x, minsplit=20))
  p_hat <- predict(fit, testset_ctree)
  y_hat2 <- ifelse(p_hat > 0.5, 1, 0) %>% factor()
  confusionMatrix(y_hat2, factor(testset$cardio))$overall[["Accuracy"]]
}
x <- seq(0.0001, 0.0009, 0.0001) #range for cp
out1 <- sapply(x, calc1)

max1 <- which.max(out1)
out1
max1
#we keep 0.0003 as the best value for cp

#We try several values for minsplit
calc2 <- function(x){
  fit <- rpart(cardio ~ .,
               data = trainset_ctree,
               control=rpart.control(cp=0.0003, minsplit=x))
  p_hat <- predict(fit, testset_ctree)
  y_hat2 <- ifelse(p_hat > 0.5, 1, 0) %>% factor()
  confusionMatrix(y_hat2, factor(testset$cardio), positive="1")$overall[["Accuracy"]]
}
x <- seq(15,35,1) #range for minsplit
out2 <- sapply(x, calc2)

max2 <- which.max(out2)
out2
max2
#we keep 15 as the best value for minsplit (in fact default -20- is the same)

#Model 2.3 : final version (parameters : cp=0.0003, minsplit=15)
fit <- rpart(cardio ~ .,
              data = trainset_ctree,
              control=rpart.control(cp=0.0003, minsplit=15))
p_hat_model23 <- predict(fit, testset_ctree)
y_hat3 <- ifelse(p_hat_model23 > 0.5, 1, 0) %>% factor()
accur_rt2 <- confusionMatrix(y_hat3, factor(testset$cardio))$overall[["Accuracy"]]
#graph
plot(fit, margin=0.1)
text(fit, cex = 0.55)


# ----------MODEL 3 : RANDOM FOREST
#only a subset is used to optimize calculation time
trainset_ctree_r <- trainset_ctree[1:5000,] 
testset_ctree_r <- testset_ctree[1:5000,]

#Model 3.1 : works with standard parameters
fit <- randomForest(cardio ~ ., data=trainset_ctree_r)
p_hat <- predict(fit, testset_ctree_r)
y_hat1 <- ifelse(p_hat > 0.5, 1, 0) %>% factor()
accur_rf1 <- confusionMatrix(y_hat1, factor(testset_ctree_r$cardio))$overall[["Accuracy"]]
#2 minutes to get the result

#Model 3.2 : we try several values for mtry (number of predictors randomly selected)
fit <- train(cardio ~ ., 
             method = "rf", 
             data = trainset_ctree_r,
             ntree=100,
             tuneGrid = data.frame(mtry = c(1,2,3,4)), 
             nodesize = 20)

p_hat <- predict(fit, testset_ctree_r)
y_hat2 <- ifelse(p_hat > 0.5, 1, 0) %>% factor()
confusionMatrix(y_hat2, factor(testset_ctree_r$cardio))$overall[["Accuracy"]]

fit$bestTune
#we keep mtry=2

#we try to optimize the parameter nodesize
calc3 <- function(x){ 
  train(cardio ~ ., 
        method = "rf", 
        data = trainset_ctree_r,
        ntree=50,
        tuneGrid = data.frame(mtry=2),
        nodesize = x)
  y_hat2 <- ifelse(p_hat > 0.5, 1, 0) %>% factor()
  confusionMatrix(y_hat2, factor(testset_ctree_r$cardio), positive="1")$overall[["Accuracy"]]
  }

x <- seq(1,101,20)
out3 <- sapply(x,calc3)

max3 <- which.max(out3)
out3
max3
#we keep nodesize=20

#Model 3.3 : final version (ntree=100, mtry=2, nodesize=20)
fit <- train(cardio ~ ., 
             method = "rf", 
             data = trainset_ctree,
             ntree=100,
             tuneGrid = data.frame(mtry=2), 
             nodesize = 20)

p_hat_model33 <- predict(fit, testset_ctree)
y_hat2 <- ifelse(p_hat_model33 > 0.5, 1, 0) %>% factor()
accur_rf2 <- confusionMatrix(y_hat2, factor(testset_ctree$cardio))$overall[["Accuracy"]]


#Accuracy of models 1,2 and 3 to prepare calculations of model4
accur_lr
accur_rt1
accur_rt2
accur_rf1
accur_rf2


# ----------MODEL 4 : ENSEMBLE
p_hat_avg <- (p_hat_model1 + p_hat_model33) /2
y_hat_avg <- ifelse(p_hat_avg > 0.5, 1, 0) %>% factor()
accur_ens <- confusionMatrix(y_hat_avg, factor(testset_ctree$cardio))$overall[["Accuracy"]]

accur_ens


# ------------------------------RESULT SECTION
# sources : datasets train and validation (from original dataset)
# train and validation already hold a column 'testbp'
# to manage blood pressure out of range values

set.seed(10, sample.kind = "Rounding")

ztrain <- train %>% filter(testbp==3) #train dataset with blood pressure normal values
zvalid <- validation %>% filter(testbp==3) #validation dataset with blood pressure normal values

#as above, score is the sum of cholesterol + gluc + smoke + alco
score <- ztrain$cholesterol + ztrain$gluc + ztrain$smoke + ztrain$alco
ztrain <- data.frame(ztrain, score) #score is added to ztrain
#we keep only features used in our model
ztrain <- ztrain %>% select(id, age, gender, weight, ap_hi, ap_lo, active, cardio, score)

score <- zvalid$cholesterol + zvalid$gluc + zvalid$smoke + zvalid$alco
zvalid <- data.frame(zvalid, score) #score is added to zvalid
#we keep only features used in our model
zvalid <- zvalid %>% select(id, age, gender, weight, ap_hi, ap_lo, active, cardio, score)

#Step 1 : Linear Regression
#Predictors are :
#age, gender, weight, ap_hi and ap_lo, physical activity & score of risk (defined above)
#our hypothesis is also that height is not linked to heart diseases in general
fit1 <- lm(cardio ~ age + gender + weight + ap_hi + ap_lo + active + score, ztrain)
p_hat1 <- predict(fit1, zvalid)
y_hat1 <- ifelse(p_hat1 > 0.5, 1, 0) %>% factor()
prec1 <- confusionMatrix(y_hat1, factor(zvalid$cardio))$overall[["Accuracy"]]
prec1

#Step 2 : Random Forest
fit2 <- train(cardio ~ ., 
             method = "rf", 
             data = ztrain,
             ntree=100,
             tuneGrid = data.frame(mtry=2), 
             nodesize = 20)
p_hat2 <- predict(fit2, zvalid)
y_hat2 <- ifelse(p_hat2 > 0.5, 1, 0) %>% factor()
prec2 <- confusionMatrix(y_hat2, factor(zvalid$cardio))$overall[["Accuracy"]]
prec2

#Step 3 : Ensemble
p_hat_avg <- (p_hat1 + p_hat2) /2
y_hat_avg <- ifelse(p_hat_avg > 0.5, 1, 0) %>% factor()

#Step 4 : Evaluation : Accuracy
#Overall accuracy
precision1 <- confusionMatrix(y_hat_avg, factor(zvalid$cardio))
precision1
precision1$overall[["Accuracy"]]

#RMSE
#calculation with cardio and predicted values before rounding (p_hat_avg)
rmserr <- RMSE(zvalid$cardio, p_hat_avg)
rmserr


###





























