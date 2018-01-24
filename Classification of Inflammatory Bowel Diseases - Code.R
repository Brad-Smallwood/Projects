# Statistics 440: Case Study 1
# Brad Smallwood, 301228034

# In collaboration with Barinder Thind, Matthew Reyers

# Install packages

# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("randomForest")
# install.packages("caret")
# install.packages("xgboost") 
#install.packages("AUC")
#install.packages("broom")

library(ggplot2)
library(dplyr)
library(stringr)
library(xgboost)
library(caret)
library(randomForest)
library(gbm)
library(AUC)
library(broom)


# Data Cleaning

setwd("C:/Users/Brad_/Desktop/SFU/Statistics/Statistics 440/Case Studies")

# Import Data
data = read.xlsx("ssc_case_study_2_inflammatory_bowel_disease.xlsx", sheetIndex = 1, as.is = TRUE)
genes = read.xlsx("ssc_case_study_2_inflammatory_bowel_disease.xlsx", sheetIndex = 2, as.is = TRUE)

head(data)
View(data)


# Documentation
# Data - Column 1 = Gene ID, Column 2 = Demographic Data, Columns 3 to 126 = information for a single patient.
# Each patient has one of three diseases: Normal, Ulcerative Colitis (UC), and Crohn's Disease (CD).


# Cleaning Issue 2: Patient names are super long, and redundant.
dim(data)
head(data)
names(data)[1] = "GeneID"
names(data)[2] = "Variable"
names(data)[c(-1,-2)] = paste0("Patient", 1:(ncol(data) - 2))


# Cleaning Issue 3: Variable names are split across two columns.
data[,1:2]
data$allvars
data$allvars = ""
data$allvars[1:4] = as.character(data[1:4,2])
data$allvars[-(1:4)] = as.character(data[-(1:4),1])
data[,2] = data$allvars
data[,1] = NULL
data$allvars = NULL
data$Patient127 = NULL


# Cleaning Issue 4: Long format.
install.packages("data.table")
library(data.table)

data2 = transpose(data)
head(data2)

View(data2)


# Cleaning Issue 5: Variable names and row names and Column Names
row.names(data)
head(data2)


# Fix first four column names
colnames(data2)[1] = "Group"
colnames(data2)[2] = "Age"
colnames(data2)[3] = "Ethnicity"
colnames(data2)[4] = "Sex"

# Fix all column names at same time.

for(i in 1:ncol(data2)){
  colnames(data2)[i] = data2[1,i]
}

# Remove useless first row of data
data2 = data2[-c(1),]

# Fix row names.
rownames(data2) <- 1:nrow(data2)
row.names(data2) = paste0("Patient", 1:nrow(data2))

View(data2)

# Cleaning Issue 6:  Numeric vs Character vs Factor

#  Convert to factors
#  Corresponds to Group, Ethnicity, Sex
for(k in c(1,3,4)){
  data2[ ,k] = as.factor(data2[ ,k])
}

# Convert to numeric
data2[ ,2] = as.numeric(data2[ ,2]) # Convert column 2 (Age) on its own.
# Convert all the rest
for(k in 5:ncol(data2)){
  data2[ ,k] = as.numeric(data2[ ,k])
}

str(data2)

# Save Data
write.csv(data2, "Case Study IBD Cleaned 1.csv")
View(data2)

x = read.csv("Case Study IBD Cleaned 1.csv")
which(x$Ethnicity == "asian")
which(x$Ethnicity == "hispanic")
which(x$Ethnicity == "indian")
#

levels(data2$Group)
levels(data2$Ethnicity)

misspelled = which(data2$Ethnicity == "cacuasian")
data2$Ethnicity[misspelled] = "caucasian"

misspelled = which(data2$Group == "Ulcerative")
data2$Group[misspelled] = "Ulcerative Colitis"


# Use to remove those levels.  
data2$Group = factor(data2$Group)
data2$Ethnicity = factor(data2$Ethnicity)
# Cleaning issue 8: Rare categories.

unique(data2$V3)

which(data2$Ethnicity == "asian")
which(data2$Ethnicity == "hispanic")
which(data2$Ethnicity == "indian")

data3 <- subset(data2, Ethnicity != "asian")
data3 <- subset(data3, Ethnicity != "indian")
data3 <- subset(data3, Ethnicity != "hispanic")
dim(data2)
dim(data3)


levels(data3$Group)
levels(data3$Ethnicity)
data3$Ethnicity = factor(data3$Ethnicity)



View(data3)
write.csv(data3, "Case Study IBD Cleaned 2.csv")



### Exploratory task 2: Distribution of gene expressions. ###

hist(data2[,5], n=15) ## Unimodal, no outliers.
hist(data2[,6], n=15) ## Unimodal, no outliers.
hist(data2[,7], n=15) ## Skewed.. some outliers.

data_genes = data2[,-c(1:4)]
means = apply(data_genes, 2, mean)
medians = apply(data_genes, 2, median)
sds = apply(data_genes, 2, sd)

pearson.skewness = function(X){
  X = X[!is.na(X)]
  mu = mean(X)
  sigma = sd(X)
  output = mean( ((X - mu) / sigma)^3)
  return(output)
}

skews = apply(data_genes, 2, pearson.skewness)

hist(means)
hist(medians)
hist(sds)
hist(skews)


which(skews > 8) ## 95, 216, and 261
skews[c(95,216,261)]

hist(data_genes[,95])
hist(data_genes[,216])
hist(data_genes[,261])

by( data_genes[,1], data2$Group, mean)
as.numeric(by(data_genes[,1], data2$Group, mean))
as.numeric(by(data_genes[,1], data2$Group, median))
as.numeric(by(data_genes[,1], data2$Group, sd))
as.numeric(by(data_genes[,1], data2$Group, pearson.skewness))

# Principle Component Analysis
install.packages("pls")
library(pls)

?pcr()

pcr.fit <- pcr(V1 ~ ., data = data2, scale = TRUE, validation = "CV")

########################################################## FILE TWO ############################################

# Load Data
setwd("C:/Users/Brad_/Desktop/SFU/Statistics/Statistics 440/Case Studies")
data <- read.csv("Case Study IBD Cleaned 2.csv")

# Loading Data issue.  Row names Screwed up
row.names(data) = paste0("Patient", 1:nrow(data))
data = data[,-c(1)] # Remove useless row.

data$Group = as.factor(as.numeric(data$Group) - 1)
data$Group
str(data$Group)

######################################################## MODEL BUILDING ###################################################

# Split data into test and train
# Randomly does this split
data2 = sample(seq_len(nrow(data)), size = floor(0.75*nrow(data)))
trainData <- data[data2, ]
testData <- data[-data2, ]

# Model 1: Random Forest Model

# Train Model
numMtry = c(14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)

rf_testResults = list()
set.seed(123)
for(i in 1:length(numMtry)){
    rf.1 <- randomForest(trainData$Group ~.,
                        data = trainData,
                        ntree = 600,
                        mtry = numMtry[i],
                        importance = TRUE,
                        proximity = TRUE)

   rf_testResults[i] = mean(rf.1$err.rate[,1]) 
}
# See that ntree ~ 600 minimizes out of bag error.

tuneRF(trainData[,2:ncol(trainData)], trainData$Group, ntreeTry = 600)

rf.1 <- randomForest(trainData$Group ~.,
                     data = trainData,
                     ntree = 600,
                     mtry = 17,
                     do.trace = TRUE,
                     importance = TRUE,
                     proximity = TRUE)

rf.1
summary(rf.1)
rf_testResults

plot(rf.1,
     main = "Random Forest Model")
varImpPlot(rf.1)


# Test Model
Pred_rf.1 <- as.vector(predict(rf.1, newdata = testData, type = "Class"))
Pred_rf.1
AccVect = (Pred_rf.1 == testData$Group)
sum(AccVect)/length(AccVect)

# Random Forest Multiple Runs
# Takes a dataset and two ingeters which determine how many times to run.
RndForest_Simulation <- function(dataset, numSims, N){
  simResults = c() # Initialize
  
  for(i in 1:N){
    # Make a new test and train set each iteration.
    data_sample = sample(seq_len(nrow(dataset)), size = floor(0.75*nrow(dataset))) 
    trainData <- dataset[data_sample, ]
    testData <- dataset[-data_sample, ]
    results = c() # Re-initialize results every iteration
    
    for(k in 1:numSims){
      rf = randomForest(trainData$Group ~ .,
                        data = trainData,
                        ntree = 600,
                        mtry = 17,
                        importance = TRUE,
                        proximity = TRUE) # Run the Random Forest
      pred_rf = as.vector(predict(rf, newdata = testData, type = "Class")) # Make prediction with test set
      LogicVec = (pred_rf == testData$Group) # See how many it got right.
      results[k] = sum(LogicVec)/length(LogicVec) # Call model accuracy function.
    }
    simResults[i] = mean(results)
  }
  return(mean(simResults))
}

rf_Results = RndForest_Simulation(data, 5, 10)
rf_Results
# Approximately 78% Accuracy

# Graphs and Plots to use
plot(rf.1)

# ROC Plot
ROC

Pred_rf_forROC <- as.vector(predict(rf.1, newdata = testData, type = "class"))
Pred_rf_forROC
classes <- levels(testData$Group)
testData$Group

pretty_colours <- c("#F8766D","#00BA38","#619CFF")
for (i in 1:3){
  # Define which observations belong to class[i]
  true_values <- ifelse(testData[,1]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(Pred_rf_forROC[i],testData$Group)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

train_rf <- train(trainData[,2:ncol(trainData)], trainData[,1], method = "rf")
train_rf
########################################## Model 2: Gradient Boost ###########################################

train_rf <- train(trainData[,2:ncol(trainData)], trainData[,1], method = "gbm")
train_rf

bestTree_gb <- gbm.perf(trainData$Group ~.,
                        data = trainData,
                        # distribution = "multinomial",
                        n.trees = 500,
                        n.minobsinnode = 100,
                        shrinkage = 0.1,
                        cv.folds = 10,
                        n.cores = 2
                        )


gb.1 <- gbm(
  trainData$Group ~.,
  data = trainData,
  distribution = "multinomial",
  var.monotone = NULL,
  n.trees = 550,
  interaction.depth = 1,
  n.minobsinnode = 10,
  shrinkage = 0.01,
  bag.fraction = 0.5,
  train.fraction = .80,
  cv.folds = 10,
  keep.data = TRUE,
  verbose = FALSE,
  n.cores = 2)


gb.1
summary(gb.1)
str(summary(gb.1))

pred_gb <- predict(gb.1, newdata = testData, n.trees = 550, type = "response")
pred_gb

predClass_gb <- apply(pred_gb, 1, which.max)
predClass_gb

AccVect = (predClass_gb == as.numeric(testData$Group))
sum(AccVect)/length(AccVect)

gm_Simulation <- function(dataset, numSims, N){
  simResults = c() # Initialize
  simCount = 0
  for(i in 1:N){
    # Make a new test and train set each iteration.
    data_sample = sample(seq_len(nrow(dataset)), size = floor(0.75*nrow(dataset))) 
    trainData <- dataset[data_sample, ]
    testData <- dataset[-data_sample, ]
    results = c() # Re-initialize results every iteration
    
    for(k in 1:numSims){
      gb = gbm(
        trainData$Group ~.,
        data = trainData,
        distribution = "multinomial",
        var.monotone = NULL,
        n.trees = 550,
        interaction.depth = 3,
        n.minobsinnode = 10,
        shrinkage = 0.01,
        bag.fraction = 0.5,
        train.fraction = 0.8,
        cv.folds = 10,
        keep.data = TRUE,
        verbose = FALSE,
        n.cores = 4) # Run the gradient boost
      
      pred_gb = predict(gb, newdata = testData, n.trees = 150, type = "response") # Make prediction with test set
      predClass_gb = apply(pred_gb, 1, which.max) # Choose the class with the highest probability
      LogicVec = (predClass_gb == as.numeric(testData$Group)) # See how many it got right.
      simCount = simCount + 1
      print(paste0(simCount, " out of ", numSims*N))
      results[k] = sum(LogicVec)/length(LogicVec) # Call model accuracy function.
      
    }
    simResults[i] = mean(results)
  }
  return(mean(simResults))
}


gb_Results = gm_Simulation(data, 5, 3)
gb_Results

testData = data.matrix((testData))


#Beginning specification.  Important!
fitControl <- trainControl(method = "repeatedcv", number = 20, repeats = 10)
fitControl

# Bunch of magic numbers.  Play around with them.
gbmGrid <-  expand.grid(interaction.depth = 4, 
                        n.trees = 1000, 
                        shrinkage = 0.01,
                        n.minobsinnode = 10)

set.seed(NULL)

# Train the model
gbmFit1 <- train(Group ~ ., data = trainData, method = "gbm", trControl = fitControl,verbose = FALSE)
gbmFit1

# Predictions
gbmFit1.Pred <- as.vector(predict(gbmFit1, newdata = testData))
gbmFit1.Pred
AccVect = (gbmFit1.Pred == testData[,1])
AccVect
sum(AccVect)/length(AccVect)


# With random data set

# Train the model
?train()
gbmFit2 <- train(Group ~ .,
                 data = trainData4,
                 method = "gbm",
                 trControl = fitControl,
                 tuneGrid = gbmGrid,
                 verbose = FALSE
)

gbmFit2

# Predictions
gbmFit2.Pred <- as.vector(predict(gbmFit2, newdata = testData4))
gbmFit2.Pred
AccVect = (gbmFit2.Pred == testData4$Group)
AccVect
model_Acc(AccVect)

gbmFit3 <- gbm(
  trainData3$Group ~.,
  data = trainData3,
  distribution = "multinomial",
  var.monotone = NULL,
  n.trees = 150,
  interaction.depth = 1,
  n.minobsinnode = 10,
  shrinkage = 0.2,
  bag.fraction = 0.5,
  train.fraction = 1.0,
  cv.folds = 0,
  keep.data = TRUE,
  verbose = FALSE,
  n.cores = NULL)

gbmFit3
summary(gbmFit3)
str(summary(gbmFit3))
View(summary(gbmFit3))

gbmFit3.Pred <- as.vector(predict(gbmFit3, newdata = testData3, n.trees = 150, type = "response"))
gbmFit3.Pred


AccVect3 = (gbmFit3.Pred == testData$Group)
AccVect3
model_Acc(AccVect3)



# Model averages 80% accuracy

#################################################################################################################
#################################################################################################################
################################################### Model 3: Extreme Gradient Boost #############################

# Set up data to be readable by xgboost

sparse_train = sparse.model.matrix()

dtrain = 
dtest = 

trainData$Group
testData$Group


################################# For Later ##############################
dtrain2 = data.matrix(trainData) # 40% accuracy with this                
dtest2 = data.matrix(testData)

dtrain2[,1] = dtrain2[,1] - 1                                            
dtest2[,1] = dtest2[,1] - 1                                                 
View(dtrain2)                                                            
##########################################################################


param <- list("objective" = "multi:softmax", # Multiple classification
              "eval_metric" = "mlogloss", # Error function.  Should me mlogloss or merror for multi-class classification
              "eta" = 0.1, # Step size shrinkage
              "max.depth" = 5, # maximum depth of tree
              "num_class" = 3 # Needs to be [0, num of classes + 1 )
             # "nthread"  = 2 # Number of cores to use.
              ) 


xgbTrain.1 <- xgb.train()

xgb.1 <- xgboost(data = dtrain,
                 max_depth = 10, # maximum depth of tree.  Typically 3-10
                 eval_metric = "mlogloss", # Needs to be this or merror for multi:softmax.
                 num_class = 3, # number of response classes + 1.
                 eta = 0.1, # Step size shrinkage
                 nthread = 2, # Number of threads to be used
                 nround = 100, # Number of trees
                 nfold = 10,
                 silent = 1,
                 objective = "multi:softmax") # Multiple classification

# Prediction
pred_xgb <- predict(xgb.1, newdata = dtest)
pred_xgb

AccVect = (pred_xgb == as.numeric(testData$Group))
AccVect

# Cross Validation with function in xgboost package

param <- list("objective" = "multi:softmax", # Multiple classification
              "eval_metric" = "mlogloss", # Error function.  Should me mlogloss or merror for multi-class classification
              "eta" = 0.1, # Step size shrinkage
              "max.depth" = 5, # maximum depth of tree
              "num_class" = 4, # Needs to be [0, num of classes + 1 )
              "nthread" = 2,
              "nround" = 100) # Number of threads to be used

xgb.dataFull = xgb.DMatrix(data.matrix(data), label = data$Group)

xgb.cv = xgb.cv(param=param, data = xgb.dataFull, nfold = 10, nrounds = 100)
summary(xgb.cv)
plot(xgb.cv$evaluation_log)

# Extreme Gradient Boost Simulation

xgboost_sim <- function(dataset, numS){ # Takes a dataset, and integer value.
  AnsVect <- c() # Initialize 
  
  for(i in 1:numS){
    # Split Data into train and test
    data = sample(seq_len(nrow(dataset)), size = floor(0.75*nrow(dataset)))
    trainData <- dataset[data, ]
    testData <- dataset[-data, ]
    
    # Train Data Setup 
    dtrain = xgb.DMatrix(data.matrix(trainData), label = trainData$Group) # Was matrix.trainData
    
    # Test Data Setup
    dtest = xgb.DMatrix(data.matrix(testData), label = testData$Group) # Was matrix.testData

    
    
    # eXtreme Gradient Boost
    xgb.fit <- xgboost(data = dtrain,
                       max_depth = 3, # maximum depth of tree
                       eval_metric = "mlogloss", # Only metric that seems to work.  Others don't make sense.
                       num_class = 4, # Number of response categories + 1
                       eta = 0.1, # Step size shrinkage
                       # nthread = 2, # Number of threads to be used (computer cores)
                       nround = 100, # Number of trees
                       nfold = 10, # Is the function cross validating on its own?
                       silent = 1,
                       objective = "multi:softmax") # Multiple classification
    
    # Predictions
    xgb_pred <- as.vector(predict(xgb.fit, newdata = dtest)) # Predictions saved as vector
    
    # print(xgb.pred)
    AccVect = (xgb_pred == as.numeric(testData$Group)) # Vector of True/False for if 

    roundAns <- sum(AccVect)/length(AccVect)
    AnsVect[i] <- roundAns
    print(roundAns)
  }
  
  # Final Answer
  finalAns <- mean(AnsVect)
  return(finalAns)
}

xgb_Results = xgboost_sim(data, 100) # May take several runs for it to not be 100% 
xgb_Results 
# Greater than 99.9% Accuracy


# Testing With Ryan

xgb.2 <- xgboost(data = dtrain2[,2:ncol(dtrain2)],
                 max_depth = 4, # maximum depth of tree.  Typically 3-10
                 eval_metric = "mlogloss", # Needs to be this or merror for multi:softmax.
                 num_class = 3, # number of response classes + 1.
                 eta = 0.1, # Step size shrinkage
                 nthread = 2, # Number of threads to be used
                 nround = 75, # Number of trees
                 nfold = 10,
                 gamma = 5,
                 silent = 1,
                 label = dtrain2[,1],
                 objective = "multi:softmax") # Multiple classification

pred_xgb2 <- predict(xgb.2, newdata = dtest2)
pred_xgb2
as.numeric(testData$Group) - 1

AccVect = (pred_xgb2 == as.numeric(testData$Group) - 1)
AccVect

mean(pred_xgb2 == as.numeric(testData$Group) - 1)


# Cross Validation with function in xgboost package

param <- list("objective" = "multi:softmax", # Multiple classification
              "eval_metric" = "mlogloss", # Error function.  Should me mlogloss or merror for multi-class classification
              "eta" = 0.1, # Step size shrinkage
              "max.depth" = 5, # maximum depth of tree
              "num_class" = 3, # Needs to be [0, num of classes + 1 )
              "nthread" = 2,
              "nround" = 100) # Number of threads to be used

str(data[,1])
data[,1] = as.numeric(data[,1]) - 1
xgb.dataFull = data.matrix(data)

xgb.crossV = xgb.cv(param=param, data = xgb.dataFull, label = data[,1], nfold = 10, nrounds = 100)
summary(xgb.crossV)
print(xgb.crossV)
plot(xgb.cv$evaluation_log)

xgb.crossV$params


data2 = sample(seq_len(nrow(data)), size = floor(0.75*nrow(data)))
trainData <- data[data2, ]
testData <- data[-data2, ]
dtrain2 = data.matrix(trainData) # 40% accuracy with this
dtest2 = data.matrix(testData)
dtrain2[,1] = dtrain2[,1] - 1
dtest2[,1] = dtest2[,1] - 1


#########################################################################################################################

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1, number = 3,
                        classProbs = TRUE,
                        allowParallel = TRUE)

xgb.grid <- expand.grid(nrounds = 1000,
                        eta = c(0.01, 0.05, 0.1),
                        max_depth = c(2,4,6,8,10,14),
                        Gamma = c(),
                        colsample_bytree = c(),
                        min_child_weight = c())

param <- list("objective" = "multi:softmax", # Multiple classification
              "eval_metric" = "mlogloss", # Error function.  Should me mlogloss or merror for multi-class classification
              "num_class" = 3) # Needs to be [0, num of classes + 1 )


vectResults = as.numeric(as.vector(as.factor(dtest2[,1])))
str(vectResults)
set.seed(45)
xgb_tune <- train(dtrain2[,2:ncol(dtrain2)],
                  vectResults,
                  data = dtrain2,
                  method = "xgbTree",
                  trControl = cv.ctrl,
                  tuneGrid = xgb.grid,
                  verbose = T,
                  metric = "Kappa",
                  nthread = 2)

dtrain2 = xgb.DMatrix(dtrain2)


# K-Mean Clustering
library(class)

trainData$Age = as.factor(as.numeric(trainData$Age))
trainData$Ethnicity = as.factor(as.numeric(trainData$Ethnicity))
trainData$Sex = as.factor(as.numeric(trainData$Sex))

testData$Age = as.factor(as.numeric(testData$Age))
testData$Ethnicity = as.factor(as.numeric(testData$Ethnicity))
testData$Sex = as.factor(as.numeric(testData$Sex))

data$Age = as.factor(as.numeric(data$Age))
data$Ethnicity = as.factor(as.numeric(data$Ethnicity))
data$Sex = as.factor(as.numeric(data$Sex))
data = data.matrix(data)

?knn()
?kmeans()
df.kmeans = data[,2:ncol(data)]
km.out <- kmeans(df.kmeans, 6, nstart = 25)
Cluster <- km.out$cluster
KMdata <- as.data.frame(cbind(data, Cluster))
colnames(KMdata)[colnames(KMdata) == 'km.out$cluster'] <- "Cluster"
View(KMdata)
KMdata[,314]
dim(KMdata)
KMdata$Group <- as.factor(KMdata$Group)
KMdata$Cluster = as.factor(KMdata$Cluster)

ggplot(KMdata,aes(x = 1:nrow(KMdata), y = KMdata$Cluster)) +
        geom_point(aes(color = KMdata$Group)) + 
        labs(title = "Patient Clusters\n", x = "Patients", y = "Clusters", color = "Patient Status") +
        scale_color_manual(labels = c("Normal", "UC", "CD"), values = c("blue", "red", "green", "purple", "cyan", "brown")) +
        ggtitle("Six Clusters") + 
        theme(plot.title = element_text(hjust = 0.5))
        # ggsave("walking_clusters_eg2.png")


#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "Elbow Plot for Clustering")


# Plots

# Silhouette Plot
install.packages("cluster")
install.packages("HSAUR")
library(cluster)
library(HSAUR)
km    <- kmeans(pottery,3)
dissE <- daisy(trainData) 
dE2   <- dissE^2
sk2   <- silhouette(km.out$cl, dE2)
plot(sk2)

# Cluster plot
install.packages("fpc")
library(fpc)
trainData = data.matrix(trainData)
plotcluster(trainData, km.out$cluster)

clusplot(trainData, km.out$cluster)
clusplot(trainData, km.out$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
