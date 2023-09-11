library(tidyverse)
library(rpart)
library(rattle)
library(rpart.plot)
library(tm)
library(slam)
library(proxy)
library(stringr)
library(textmineR)
library(igraph)
library(lsa)
library(textstem)
library(arules)
library(arulesViz)
library(naivebayes)
library(e1071)
library(caret)
library(rpart)
library(amap)
library(akmeans)
library(proxy)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext) 
library(plyr) 
library(ggplot2)
library(factoextra) 
library(networkD3)
library(functional)
library(StatMatch)

# Set WD 
setwd("/Users/david/Documents/Grad School/IST 707/Final_Project")

summerTranferData_initial <- read.csv("/Users/david/Documents/Grad School/IST 707/Final_Project/2022_2023_football_summer_transfers_v2.csv", na.string = c(""))


############################## 
##### Associative Rule Mining #####
############################## 
(str(summerTranferData_initial))

# Make ARM version of data set
arm_summerTransferData <- summerTranferData_initial

# Turn text values into factors
arm_summerTransferData$position <- as.factor(arm_summerTransferData$position)
arm_summerTransferData$age <- as.factor(arm_summerTransferData$age)
arm_summerTransferData$country_from <- as.factor(arm_summerTransferData$country_from)
arm_summerTransferData$league_from <- as.factor(arm_summerTransferData$league_from)
arm_summerTransferData$club_from <- as.factor(arm_summerTransferData$club_from)
arm_summerTransferData$country_to <- as.factor(arm_summerTransferData$country_to)
arm_summerTransferData$league_to <- as.factor(arm_summerTransferData$league_to)
arm_summerTransferData$club_to <- as.factor(arm_summerTransferData$club_to)
arm_summerTransferData$loan <- as.factor(arm_summerTransferData$loan)

# Discretize Numeric values\
str(arm_summerTransferData)
summary(arm_summerTransferData)

arm_summerTransferData$market_value <- cut(arm_summerTransferData$market_value, breaks = c(0, 1, 2.5, 5, 10, 50, Inf), labels = c("Under 1m", "1m-2.5m", "2.5m-5m", "5m-10m", "10m-50m", "50m+"))
arm_summerTransferData$fee <- cut(arm_summerTransferData$fee, breaks = c(0, 1, 2.5, 5, 10, 50, Inf), labels = c("Under 1m", "1m-2.5m", "2.5m-5m", "5m-10m", "10m-50m", "50m+"))


(str(arm_summerTransferData))

arm_summerTransferData <- arm_summerTransferData[, -1]
arm_summerTransferData <- arm_summerTransferData[, -11]


arm_apriori <- arm_summerTransferData


(str(arm_apriori))

# Convert to transactions for Apriori
arm_Transactions <- as(arm_apriori, "transactions")

arm_summerTransferData_rules <- apriori(arm_Transactions, , parameter = list(supp = 0.03, conf = 0.90, minlen = 3, maxlen = 10))

arm_summerTransferData_rules <- sort(arm_summerTransferData_rules, decreasing=TRUE,by='lift')

###
### Top Overall Market Rules
###
inspectDT(arm_summerTransferData_rules)
arules::inspect(arm_summerTransferData_rules)
plot(arm_summerTransferData_rules,method="graph",shading = "lift")




# Top Overall Market Rules part 2
arm_summerTransferData2 <- arm_summerTransferData[, -4]
arm_summerTransferData2 <- arm_summerTransferData2[, -6]

arm_apriori2 <- arm_summerTransferData2

arm_Transactions2 <- as(arm_apriori2, "transactions")

arm_summerTransferData_rules2 <- apriori(arm_Transactions2, , parameter = list(supp = 0.005, conf = 0.90, minlen = 3, maxlen = 10))

arm_summerTransferData_rules2 <- sort(arm_summerTransferData_rules2, decreasing=TRUE,by='lift')

###
### Top Overall Market Rules part 2
###
inspectDT(arm_summerTransferData_rules2)
arules::inspect(arm_summerTransferData_rules2)
plot(arm_summerTransferData_rules2,method="graph",shading = "lift")



# ARM by league
arm_apriori2 <- arm_summerTransferData

str(arm_apriori2)


arm_apriori2 <- arm_apriori2[, -4]
arm_apriori2 <- arm_apriori2[, -6]
#arm_apriori2 <- arm_apriori2[, -7]

str(arm_apriori2)

arm_Transactions2 <- as(arm_apriori2, "transactions")

###
### PL Rules
###
arm_summerTransferData_rules_PL<-  apriori(arm_Transactions2, parameter = list(supp = 0.003, conf = 0.90, minlen = 3, maxlen = 10), 
                          appearance = list(default="lhs",rhs="league_to=Premier League"))
arm_summerTransferData_rules_PL <-sort(arm_summerTransferData_rules_PL, decreasing=TRUE,by='support')

inspectDT(arm_summerTransferData_rules_PL)
arules::inspect(arm_summerTransferData_rules_PL)
plot(arm_summerTransferData_rules_PL,method="graph",shading = "lift")


###
### Bundesliga Rules
###
arm_summerTransferData_rules_Bund<-  apriori(arm_Transactions2, parameter = list(supp = 0.003, conf = 0.90, minlen = 3, maxlen = 10), 
                                           appearance = list(default="lhs",rhs="league_to=Bundesliga"))
arm_summerTransferData_rules_Bund <-sort(arm_summerTransferData_rules_Bund, decreasing=TRUE,by='support')

inspectDT(arm_summerTransferData_rules_Bund)
arules::inspect(arm_summerTransferData_rules_Bund)
plot(arm_summerTransferData_rules_Bund,method="graph",shading = "lift")

###
### Laliga Rules
###
arm_summerTransferData_rules_LaLiga<-  apriori(arm_Transactions2, parameter = list(supp = 0.003, conf = 0.90, minlen = 3, maxlen = 10), 
                                             appearance = list(default="lhs",rhs="league_to=LaLiga"))
arm_summerTransferData_rules_LaLiga <-sort(arm_summerTransferData_rules_LaLiga, decreasing=TRUE,by='support')

inspectDT(arm_summerTransferData_rules_LaLiga)
arules::inspect(arm_summerTransferData_rules_LaLiga)
plot(arm_summerTransferData_rules_LaLiga,method="graph",shading = "lift")

###
### Serie A Rules
###
arm_summerTransferData_rules_SerieA <-  apriori(arm_Transactions2, parameter = list(supp = 0.003, conf = 0.90, minlen = 3, maxlen = 10), 
                                               appearance = list(default="lhs",rhs="league_to=Serie A"))
arm_summerTransferData_rules_SerieA <-sort(arm_summerTransferData_rules_SerieA, decreasing=TRUE,by='support')

inspectDT(arm_summerTransferData_rules_SerieA)
arules::inspect(arm_summerTransferData_rules_SerieA)
plot(arm_summerTransferData_rules_SerieA,method="graph",shading = "lift")


###
### 10m - 50m Players Rules
###
arm_summerTransferData_rules_starPlayers <-  apriori(arm_Transactions2, parameter = list(supp = 0.003, conf = 0.90, minlen = 2, maxlen = 10), 
                                                appearance = list(default="lhs",rhs="fee=10m-50m"))
arm_summerTransferData_rules_starPlayers <-sort(arm_summerTransferData_rules_starPlayers, decreasing=TRUE,by='support')

inspectDT(arm_summerTransferData_rules_starPlayers)
arules::inspect(arm_summerTransferData_rules_starPlayers)
plot(arm_summerTransferData_rules_starPlayers,method="graph",shading = "lift")


############################## 
##### Naive Bayes #####
############################## 
# Make Naive Bayes version of data set
nb_summerTransferData <- summerTranferData_initial

(str(nb_summerTransferData))

# Turn text values into factors
nb_summerTransferData$position <- as.factor(nb_summerTransferData$position)
nb_summerTransferData$age <- as.factor(nb_summerTransferData$age)
nb_summerTransferData$country_from <- as.factor(nb_summerTransferData$country_from)
nb_summerTransferData$league_from <- as.factor(nb_summerTransferData$league_from)
nb_summerTransferData$club_from <- as.factor(nb_summerTransferData$club_from)
nb_summerTransferData$country_to <- as.factor(nb_summerTransferData$country_to)
nb_summerTransferData$league_to <- as.factor(nb_summerTransferData$league_to)
nb_summerTransferData$club_to <- as.factor(nb_summerTransferData$club_to)
nb_summerTransferData$loan <- as.factor(nb_summerTransferData$loan)

nb_summerTransferData$market_value <- cut(nb_summerTransferData$market_value, breaks = c(0, 1, 2.5, 5, 10, 50, Inf), labels = c("Under 1m", "1m-2.5m", "2.5m-5m", "5m-10m", "10m-50m", "50m+"))
nb_summerTransferData$fee <- cut(nb_summerTransferData$fee, breaks = c(0, 1, 2.5, 5, 10, 50, Inf), labels = c("Under 1m", "1m-2.5m", "2.5m-5m", "5m-10m", "10m-50m", "50m+"))

nb_summerTransferData <- nb_summerTransferData[, -1]

(str(nb_summerTransferData))


set.seed(25)
sample <- sample(c(TRUE, FALSE), nrow(nb_summerTransferData), replace = TRUE, prob = c(0.8, 0.2))
nb_summerTransferData_train <- nb_summerTransferData[sample,]
nb_summerTransferData_test<- nb_summerTransferData[!sample,]


## Make sure label is factor type
str(nb_summerTransferData_test)
## Copy the Labels
(transfer_Test_Labels <- nb_summerTransferData_test[,10])
str(transfer_Test_Labels)
## Remove the labels
nb_summerTransferData_test_NO_LABEL <- nb_summerTransferData_test[,-c(10)]

## using naivebayes package
NB_model_summerTransfer <- naive_bayes(nb_summerTransferData_train$fee ~ ., data=nb_summerTransferData_train, laplace = 1)
NB_prediction_summerTransfer <- predict(NB_model_summerTransfer, nb_summerTransferData_test_NO_LABEL)
table(NB_prediction_summerTransfer,transfer_Test_Labels)

plot(NB_prediction_summerTransfer)
(conf.mat <- confusionMatrix(NB_prediction_summerTransfer, transfer_Test_Labels))

############ Predict League Transfer To ############ 
nb_summerTransferData <- summerTranferData_initial

nb_summerTransferData$market_value <- cut(nb_summerTransferData$market_value, breaks = c(0, 1, 2.5, 5, 10, 50, Inf), labels = c("Under 1m", "1m-2.5m", "2.5m-5m", "5m-10m", "10m-50m", "50m+"))
nb_summerTransferData$fee <- cut(nb_summerTransferData$fee, breaks = c(0, 1, 2.5, 5, 10, 50, Inf), labels = c("Under 1m", "1m-2.5m", "2.5m-5m", "5m-10m", "10m-50m", "50m+"))

nb_summerTransferData <- nb_summerTransferData[, -1]

top_leagues <- c("Premier League", "LaLiga", "Bundesliga", "Serie A", "Ligue 1")

nb_summerTransferData <- nb_summerTransferData[is.element(nb_summerTransferData$league_to, top_leagues),]

str(nb_summerTransferData)

nb_summerTransferData$position <- as.factor(nb_summerTransferData$position)
nb_summerTransferData$age <- as.factor(nb_summerTransferData$age)
nb_summerTransferData$country_from <- as.factor(nb_summerTransferData$country_from)
nb_summerTransferData$league_from <- as.factor(nb_summerTransferData$league_from)
nb_summerTransferData$club_from <- as.factor(nb_summerTransferData$club_from)
nb_summerTransferData$country_to <- as.factor(nb_summerTransferData$country_to)
nb_summerTransferData$league_to <- as.factor(nb_summerTransferData$league_to)
nb_summerTransferData$club_to <- as.factor(nb_summerTransferData$club_to)
nb_summerTransferData$loan <- as.factor(nb_summerTransferData$loan)


set.seed(25)
sample <- sample(c(TRUE, FALSE), nrow(nb_summerTransferData), replace = TRUE, prob = c(0.8, 0.2))
nb_summerTransferData_train_leagues <- nb_summerTransferData[sample,]
nb_summerTransferData_test_leagues<- nb_summerTransferData[!sample,]

## Make sure label is factor type
str(nb_summerTransferData_test_leagues)
## Copy the Labels
(transfer_Test_Labels_league <- nb_summerTransferData_test_leagues[,8])
str(transfer_Test_Labels_league)
## Remove the labels
nb_summerTransferData_test_League_NO_LABEL <- nb_summerTransferData_test_leagues[,-c(8)]

## using naivebayes package
NB_model_summerTransfer_league <- naive_bayes(nb_summerTransferData_train_leagues$league_to ~ ., data=nb_summerTransferData_train_leagues, laplace = 1)
NB_prediction_summerTransfer_league <- predict(NB_model_summerTransfer_league, nb_summerTransferData_test_League_NO_LABEL)
table(NB_prediction_summerTransfer_league,transfer_Test_Labels_league)

plot(NB_prediction_summerTransfer_league)
(conf.mat <- confusionMatrix(NB_prediction_summerTransfer_league, transfer_Test_Labels_league))

############################## 
##### SVM #####
############################## 
# Make SVM version of data set
svm_summerTransferData <- summerTranferData_initial

(str(svm_summerTransferData))

# Turn text values into factors
svm_summerTransferData$position <- as.factor(svm_summerTransferData$position)
svm_summerTransferData$age <- as.factor(svm_summerTransferData$age)
svm_summerTransferData$country_from <- as.factor(svm_summerTransferData$country_from)
svm_summerTransferData$league_from <- as.factor(svm_summerTransferData$league_from)
svm_summerTransferData$club_from <- as.factor(svm_summerTransferData$club_from)
svm_summerTransferData$country_to <- as.factor(svm_summerTransferData$country_to)
svm_summerTransferData$league_to <- as.factor(svm_summerTransferData$league_to)
svm_summerTransferData$club_to <- as.factor(svm_summerTransferData$club_to)
svm_summerTransferData$loan <- as.factor(svm_summerTransferData$loan)

svm_summerTransferData$market_value <- cut(svm_summerTransferData$market_value, breaks = c(0, 1, 2.5, 5, 10, 50, Inf), labels = c("Under 1m", "1m-2.5m", "2.5m-5m", "5m-10m", "10m-50m", "50m+"))
svm_summerTransferData$fee <- cut(svm_summerTransferData$fee, breaks = c(0, 1, 2.5, 5, 10, 50, Inf), labels = c("Under 1m", "1m-2.5m", "2.5m-5m", "5m-10m", "10m-50m", "50m+"))

svm_summerTransferData <- svm_summerTransferData[, -1]
svm_summerTransferData <- svm_summerTransferData[, -6]
svm_summerTransferData <- svm_summerTransferData[, -8]

(str(svm_summerTransferData))



set.seed(4)
sample <- sample(c(TRUE, FALSE), nrow(svm_summerTransferData), replace = TRUE, prob = c(0.8, 0.2))
svm_summerTransferData_train <- svm_summerTransferData[sample,]
svm_summerTransferData_test<- svm_summerTransferData[!sample,]

svm_summerTransferData_train <- svm_summerTransferData_test[!is.na(svm_summerTransferData_test$fee),]
svm_summerTransferData_train <- svm_summerTransferData_test[!is.na(svm_summerTransferData_test$market_value),]

svm_summerTransferData_test <- svm_summerTransferData_test[!is.na(svm_summerTransferData_test$fee),]
svm_summerTransferData_test <- svm_summerTransferData_test[!is.na(svm_summerTransferData_test$market_value),]

str(svm_summerTransferData_test)
## Copy the Labels
(svm_summerTransfer_Test_Labels <- svm_summerTransferData_test[,8])
str(svm_summerTransfer_Test_Labels)
## Remove the labels
(svm_summerTransferData_test_NO_LABEL <- svm_summerTransferData_test[,-c(8)])

SVM_polynomial_summerTransfer <- svm(fee~., data=svm_summerTransferData_train,
                        kernel="polynomial", cost=3,
                        scale=FALSE)
print(SVM_polynomial_summerTransfer)

## Confusion Matrix for training data to check model
SVM_polynomial_summerTransfer_pred <- predict(SVM_polynomial_summerTransfer, svm_summerTransferData_test, type="class")
(SVM_polynomial_summerTransfer_pred)

plot(SVM_polynomial_summerTransfer_pred)
(confusionMatrix(SVM_polynomial_summerTransfer_pred, svm_summerTransferData_test$fee))


#### SVM League Model
svm_summerTransferData <- summerTranferData_initial

(str(svm_summerTransferData))

svm_summerTransferData <- svm_summerTransferData[is.element(svm_summerTransferData$league_to, top_leagues),]

# Turn text values into factors
svm_summerTransferData$position <- as.factor(svm_summerTransferData$position)
svm_summerTransferData$age <- as.factor(svm_summerTransferData$age)
svm_summerTransferData$country_from <- as.factor(svm_summerTransferData$country_from)
svm_summerTransferData$league_from <- as.factor(svm_summerTransferData$league_from)
svm_summerTransferData$club_from <- as.factor(svm_summerTransferData$club_from)
svm_summerTransferData$country_to <- as.factor(svm_summerTransferData$country_to)
svm_summerTransferData$league_to <- as.factor(svm_summerTransferData$league_to)
svm_summerTransferData$club_to <- as.factor(svm_summerTransferData$club_to)
svm_summerTransferData$loan <- as.factor(svm_summerTransferData$loan)

svm_summerTransferData$market_value <- cut(svm_summerTransferData$market_value, breaks = c(0, 1, 2.5, 5, 10, 50, Inf), labels = c("Under 1m", "1m-2.5m", "2.5m-5m", "5m-10m", "10m-50m", "50m+"))
svm_summerTransferData$fee <- cut(svm_summerTransferData$fee, breaks = c(0, 1, 2.5, 5, 10, 50, Inf), labels = c("Under 1m", "1m-2.5m", "2.5m-5m", "5m-10m", "10m-50m", "50m+"))

svm_summerTransferData <- svm_summerTransferData[, -1]
svm_summerTransferData <- svm_summerTransferData[, -6]
svm_summerTransferData <- svm_summerTransferData[, -8]

(str(svm_summerTransferData))

set.seed(4)
sample <- sample(c(TRUE, FALSE), nrow(svm_summerTransferData), replace = TRUE, prob = c(0.8, 0.2))
svm_summerTransferData_train <- svm_summerTransferData[sample,]
svm_summerTransferData_test<- svm_summerTransferData[!sample,]

svm_summerTransferData_train <- svm_summerTransferData_train[!is.na(svm_summerTransferData_train$fee),]
svm_summerTransferData_train <- svm_summerTransferData_train[!is.na(svm_summerTransferData_train$market_value),]

svm_summerTransferData_test <- svm_summerTransferData_test[!is.na(svm_summerTransferData_test$fee),]
svm_summerTransferData_test <- svm_summerTransferData_test[!is.na(svm_summerTransferData_test$market_value),]

svm_summerTransferData_train_league <- svm_summerTransferData_train 
svm_summerTransferData_test_league <- svm_summerTransferData_test

(svm_summerTransfer_Test_Labels_League <- svm_summerTransferData_test_league[,7])
str(svm_summerTransfer_Test_Labels_League)
## Remove the labels
(svm_summerTransferData_test_NO_LABEL_League <- svm_summerTransferData_test_league[,-c(7)])

SVM_polynomial_summerTransfer_league <- svm(league_to~., data=svm_summerTransferData_train_league,
                                     kernel="polynomial", cost=3,
                                     scale=FALSE)
print(SVM_polynomial_summerTransfer_league)

## Confusion Matrix for training data to check model
SVM_polynomial_summerTransfer_pred_league <- predict(SVM_polynomial_summerTransfer_league, svm_summerTransferData_test_league, type="class")
(SVM_polynomial_summerTransfer_pred_league)

plot(SVM_polynomial_summerTransfer_pred_league)
(confusionMatrix(SVM_polynomial_summerTransfer_pred_league, svm_summerTransferData_test_league$league_to))


############################## 
##### Clustering/Classification #####
############################## 
# Make Clustering version of data set
cluster_summerTransferData <- summerTranferData_initial

(str(cluster_summerTransferData))

# Turn text values into factors
cluster_summerTransferData$position <- as.factor(cluster_summerTransferData$position)
cluster_summerTransferData$age <- as.factor(cluster_summerTransferData$age)
cluster_summerTransferData$country_from <- as.factor(cluster_summerTransferData$country_from)
cluster_summerTransferData$league_from <- as.factor(cluster_summerTransferData$league_from)
cluster_summerTransferData$club_from <- as.factor(cluster_summerTransferData$club_from)
cluster_summerTransferData$country_to <- as.factor(cluster_summerTransferData$country_to)
cluster_summerTransferData$league_to <- as.factor(cluster_summerTransferData$league_to)
cluster_summerTransferData$club_to <- as.factor(cluster_summerTransferData$club_to)



#cluster_summerTransferData <- cluster_summerTransferData[, -12]
cluster_summerTransferData <- cluster_summerTransferData[, -10]
cluster_summerTransferData <- cluster_summerTransferData[, -8]
cluster_summerTransferData <- cluster_summerTransferData[, -7]
cluster_summerTransferData <- cluster_summerTransferData[, -5]
#cluster_summerTransferData <- cluster_summerTransferData[, -4]
cluster_summerTransferData <- cluster_summerTransferData[, -3]
cluster_summerTransferData <- cluster_summerTransferData[, -2]

cluster_summerTransferData <- cluster_summerTransferData[!is.na(cluster_summerTransferData$fee),]


# PL Cluster
cluster_summerTransferData_PL <- cluster_summerTransferData[cluster_summerTransferData$league_to == "Premier League",]
cluster_summerTransferData_PL <- cluster_summerTransferData_PL[cluster_summerTransferData_PL$loan == "False",]
cluster_summerTransferData_PL$loan <- as.factor(cluster_summerTransferData_PL$loan)
#cluster_summerTransferData$fee <- cut(cluster_summerTransferData$fee, breaks = c(0, 1, 2.5, 5, 10, 50, Inf), labels = c("Under 1m", "1m-2.5m", "2.5m-5m", "5m-10m", "10m-50m", "50m+"))

cluster_summerTransferData_PL <- cluster_summerTransferData_PL[complete.cases(cluster_summerTransferData_PL),]

(str(cluster_summerTransferData_PL))

# Label Matrix
## Copy the Labels
(cluster_summerTransfer_Labels <- cluster_summerTransferData_PL[,1])
str(cluster_summerTransfer_Labels)
## Remove the labels
(cluster_summerTransferData_PL <- cluster_summerTransferData_PL[,-c(1)])

cluster_summerTransferData_Matrix <- as.matrix(cluster_summerTransferData_PL)

gower_cluster_summerTransferData_Matrix <- gower.dist(cluster_summerTransferData_PL)

rownames(gower_cluster_summerTransferData_Matrix) <- cluster_summerTransfer_Labels

# Run Kmeans multiple times
summerTransfer_euc_kmeans <- Kmeans(gower_cluster_summerTransferData_Matrix, centers=5,method = "euclidean")
fviz_cluster(summerTransfer_euc_kmeans, gower_cluster_summerTransferData_Matrix, main="Euclidean")

summerTransfer_manhattan_kmeans <- Kmeans(gower_cluster_summerTransferData_Matrix, centers=5,method = "manhattan")
fviz_cluster(summerTransfer_manhattan_kmeans, gower_cluster_summerTransferData_Matrix, main="Manhattan")


# Star Player Cluster
str(cluster_summerTransferData)

cluster_summerTransferData_stars <- cluster_summerTransferData[cluster_summerTransferData$fee >= 20,]
cluster_summerTransferData_stars <- cluster_summerTransferData_stars[cluster_summerTransferData_stars$loan == "False",]
cluster_summerTransferData_stars$loan <- as.factor(cluster_summerTransferData_stars$loan)
#cluster_summerTransferData$fee <- cut(cluster_summerTransferData$fee, breaks = c(0, 1, 2.5, 5, 10, 50, Inf), labels = c("Under 1m", "1m-2.5m", "2.5m-5m", "5m-10m", "10m-50m", "50m+"))

cluster_summerTransferData_stars <- cluster_summerTransferData_stars[complete.cases(cluster_summerTransferData_stars),]

(str(cluster_summerTransferData_stars))

# Label Matrix
## Copy the Labels
str(cluster_summerTransferData_stars)
(cluster_summerTransfer_Labels_stars <- cluster_summerTransferData_stars[,1])
str(cluster_summerTransfer_Labels_stars)
## Remove the labels
(cluster_summerTransferData_stars <- cluster_summerTransferData_stars[,-c(1)])

gower_cluster_summerTransferData_Matrix_stars <- gower.dist(cluster_summerTransferData_stars)

rownames(gower_cluster_summerTransferData_Matrix_stars) <- cluster_summerTransfer_Labels_stars

# Run Kmeans multiple times
summerTransfer_euc_kmeans_stars <- Kmeans(gower_cluster_summerTransferData_Matrix_stars, centers=4,method = "euclidean")
fviz_cluster(summerTransfer_euc_kmeans_stars, gower_cluster_summerTransferData_Matrix_stars, main="Euclidean")

summerTransfer_manhattan_kmeans_stars <- Kmeans(gower_cluster_summerTransferData_Matrix_stars, centers=5,method = "manhattan")
fviz_cluster(summerTransfer_manhattan_kmeans_stars, gower_cluster_summerTransferData_Matrix_stars, main="Manhattan")

