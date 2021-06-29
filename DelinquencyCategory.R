# =============================
# Class: STAT 515
# Group Members: Gayathri, Padma and Akbar Jiwani
# Final Project 
# Due Date: May 2020
# Objective: The primary objective of this analytical exercise is to predict the type of delinquency category using 26 of the 31 available columns
# =============================

.libPaths("c:/R Programming/library")
.libPaths()

# set working directory
setwd("C:/R Project/working_directory")
getwd()

#install.packages("h2o")
library(tidyverse)
library(dplyr)
library(h2o)
library(ggcorrplot)
library(reshape2)
library(ggthemes)
library(lattice)
library(VIM)
library(ggcorrplot)
library(reshape2)
library(ggthemes)
library(randomForest)
library(nnet)
library('MASS')

rm(list=ls())
#clear the console
cat("\014")

# @@@@@@@@@@@@@@@@@@ READ THE DATA @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Objective: Read the Collateral Loan Data
collateralLoans <- as_tibble(read.csv("CollateralLoanData.csv", header = TRUE, na.strings = c("", "NA")))
collateralLoans

# @@@@@@@@@@@@@@@@@@ DATA EXPLORATION @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# we can safely remove LtvRatioPercent as we can use LtvRatioPercentCurrent
collateralLoans <- dplyr::select(collateralLoans, -c("LtvRatioPercent"))

totalRows <- nrow(collateralLoans)
ncol(collateralLoans)

#merge REO to Foreclosure
#str(collateralLoans$DelinquencyCategory)
collateralLoans$DelinquencyCategory <- recode_factor(collateralLoans$DelinquencyCategory, "In Foreclosure" = "Foreclosure")
collateralLoans$DelinquencyCategory <- recode_factor(collateralLoans$DelinquencyCategory, "REO" = "Foreclosure")
collateralLoans$NoteDate <- as.Date(collateralLoans$NoteDate, format="%m/%d/%Y")
collateralLoans$PropertyValuationEffectiveDate <- as.Date(collateralLoans$PropertyValuationEffectiveDate, format="%m/%d/%Y")
collateralLoans$ScheduledFirstPaymentDate <- as.Date(collateralLoans$ScheduledFirstPaymentDate, format="%m/%d/%Y")
collateralLoans$LastPaidInstallmentDueDate <- as.Date(collateralLoans$LastPaidInstallmentDueDate, format="%m/%d/%Y")
collateralLoans$LoanMaturityDate <- as.Date(collateralLoans$LoanMaturityDate, format="%m/%d/%Y")
collateralLoans$NextPayDate <- as.Date(collateralLoans$NextPayDate, format="%m/%d/%Y")
collateralLoans$LastPaymentReceivedDate <- as.Date(collateralLoans$LastPaymentReceivedDate, format="%m/%d/%Y")

summary(collateralLoans)
#str(collateralLoans)

# replace values in IsArm column, replace False to 0 and true to 1
IsArm <- rep(1,totalRows)
IsArm[collateralLoans$IsArm == "FALSE"] <- 0
collateralLoans$IsArm <- IsArm

IsCurrentlyIo <- rep(1,totalRows)
IsCurrentlyIo[collateralLoans$IsCurrentlyIo == "False"] <- 0
collateralLoans$IsCurrentlyIo <- IsCurrentlyIo

na.count <- as.data.frame(map(collateralLoans, ~sum(is.na(.))))
na.count.melt <- melt(na.count)
na.count.melt

response.variable <- "DelinquencyCategory"
predictor.variables <- setdiff(names(collateralLoans), response.variable)
predictor.variables


g <- ggplot(data=collateralLoans, aes(CurrentLtvRange, fill=CurrentLtvRange))
g + geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5), plot.title = element_text(size = 11) ) +
  scale_fill_manual(values = c("bisque4", "cadetblue4", "darkkhaki", "orange2","bisque3", "dodgerblue4", "lightsteelblue4", "thistle4"))
  
#boxplots - show outliers
#1. NoteAmount
g <- ggplot(data=collateralLoans, aes(y=NoteAmount, x = DelinquencyCategory))
g + geom_boxplot(aes(fill=DelinquencyCategory))

g <- ggplot(data=collateralLoans, aes(x=NoteAmount))
g + geom_histogram(aes(y=..density..),color="black", fill="grey") +
  geom_density(alpha=.2, fill="red") +
  theme_light()

#2. CurrentBalance
g <- ggplot(data=collateralLoans, aes(y=CurrentBalance, x = DelinquencyCategory))
g + geom_boxplot(aes(fill=DelinquencyCategory))

g <- ggplot(data=collateralLoans, aes(x=CurrentBalance))
g + geom_histogram(aes(y=..density..),color="black", fill="grey") +
  geom_density(alpha=.2, fill="red") +
  theme_light()
  
#3. PropertyValuationAmount
g <- ggplot(data=collateralLoans, aes(y=PropertyValuationAmount, x = DelinquencyCategory))
g + geom_boxplot(aes(fill=DelinquencyCategory))

g <- ggplot(data=collateralLoans, aes(x=PropertyValuationAmount))
g + geom_histogram(aes(y=..density..),color="black", fill="grey") +
  geom_density(alpha=.2, fill="red") +
  theme_light()

#4. CreditScore
g <- ggplot(data=collateralLoans, aes(y=CreditScoreValueRecent, x = DelinquencyCategory))
g + geom_boxplot(aes(fill=DelinquencyCategory))

g <- ggplot(data=collateralLoans, aes(x=CreditScoreValueRecent))
g + geom_histogram(aes(y=..density..),color="black", fill="grey") +
  geom_density(alpha=.2, fill="red") +
  theme_light()

# @@@@@@@@@@@@@@@@@@ DATA TRANSFORMATION @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# normalize NoteAmount, CurrentBalance and PropertyValuationAmount
collateralLoans$NoteAmount <- log(collateralLoans$NoteAmount)
collateralLoans$CurrentBalance <- log(collateralLoans$CurrentBalance)
collateralLoans$PropertyValuationAmount <- log(collateralLoans$PropertyValuationAmount)

#1. NoteAmount
g <- ggplot(data=collateralLoans, aes(y=NoteAmount, x = DelinquencyCategory))
g + geom_boxplot(aes(fill=DelinquencyCategory))

g <- ggplot(data=collateralLoans, aes(x=NoteAmount))
g + geom_histogram(aes(y=..density..),color="black", fill="grey") +
  geom_density(alpha=.2, fill="red") +
  theme_light()

shapiro.test(collateralLoans$NoteAmount)

#2. CurrentBalance
g <- ggplot(data=collateralLoans, aes(y=CurrentBalance, x = DelinquencyCategory))
g + geom_boxplot(aes(fill=DelinquencyCategory))

g <- ggplot(data=collateralLoans, aes(x=CurrentBalance))
g + geom_histogram(aes(y=..density..),color="black", fill="grey") +
  geom_density(alpha=.2, fill="red") +
  theme_light()

shapiro.test(collateralLoans$CurrentBalance)

#3. PropertyValuationAmount
g <- ggplot(data=collateralLoans, aes(y=PropertyValuationAmount, x = DelinquencyCategory))
g + geom_boxplot(aes(fill=DelinquencyCategory))

g <- ggplot(data=collateralLoans, aes(x=PropertyValuationAmount))
g + geom_histogram(aes(y=..density..),color="black", fill="grey") +
  geom_density(alpha=.2, fill="red") +
  theme_light()

shapiro.test(collateralLoans$PropertyValuationAmount)

# @@@@@@@@@@@@@@@@@@ TRAINING AND PREDICTION @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#collateralLoans.rf <- collateralLoans

collateralLoans.rf <- dplyr::select(collateralLoans, c("DelinquencyCategory","MonthsDq", "LastPaymentReceivedDate", "PaymentType", "TimesDq", "ServicerIdentifier", "StateCode",
                                                 "LtvRatioPercentCurrent", "CurrentBalance", "NoteRatePercent", "NoteAmount", "Age",
                                                 "PropertyValuationEffectiveDate","CreditScoreValueRecent"))


set.seed(1234)
sample.training <- sample.int(n = nrow(collateralLoans.rf), size = floor(.75*nrow(collateralLoans.rf)), replace = F)
collateralLoans.training <- collateralLoans.rf[sample.training,]
collateralLoans.validation <- collateralLoans.rf[-sample.training,]

#str(collateralLoans.training)

# @@@@@@@@@@@@@@@@@@ 1. RANDOM FOREST
collateralLoans.fit.rf <- randomForest(data=collateralLoans.training, DelinquencyCategory~., importance=TRUE, ntrees=1000,mtry=4)
collateralLoans.fit.rf

importance <- round(importance(collateralLoans.fit.rf), 2)
importance

mean.decrease.accuracy.importance <- data.frame(Variables =row.names(importance),Importance=round(importance[,"MeanDecreaseAccuracy"],2))
mean.decrease.accuracy.importance

mean.decrease.accurance.rankimprtance <- mean.decrease.accuracy.importance %>% mutate(Rank=paste('#',dense_rank(desc(Importance))))
g <- ggplot(mean.decrease.accurance.rankimprtance, aes(x=reorder(Variables,Importance), y=Importance,fill=Importance))
g + geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),hjust=0, vjust=0.55, size = 2.5, colour = 'white') +
  labs(x = 'Variables') + coord_flip() + 
  ggtitle("Mean Decrease Accuracy-Variable Importance from Random Forest")

mean.decrease.gini.importance <- data.frame(Variables =row.names(importance),Importance=round(importance[,"MeanDecreaseGini"],2))
mean.decrease.gini.rankimportance <- mean.decrease.gini.importance %>% mutate(Rank=paste('#',dense_rank(desc(Importance))))

g <- ggplot(mean.decrease.gini.rankimportance, aes(x=reorder(Variables,Importance),y=Importance,fill=Importance))
g + geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),hjust=0, vjust=0.55, size = 2.5, colour = 'white') +
  labs(x = 'Variables') + 
  coord_flip() + 
  ggtitle("Mean Decrease Gini-Variable Importance from Random Forest")


collateralLoans.predict.rf <- predict(collateralLoans.fit.rf, newdata = collateralLoans.validation, type = "response")

confusionMatrix.rf <- table(collateralLoans.validation$DelinquencyCategory, collateralLoans.predict.rf)
confusionMatrix.rf
accuracy.rate.rf <- sum(diag(confusionMatrix.rf)) / sum(confusionMatrix.rf)
accuracy.rate.rf * 100

misclassification.rate.rf <- 100 - (accuracy.rate.rf * 100)
misclassification.rate.rf

# @@@@@@@@@@@@@@@@@@ 2. GBM - Gradient Boosting Machine
set.seed(412)
#?h2o.grid()
local.h2o <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, nthreads=-1)

collateralLoans.gbm <- dplyr::select(collateralLoans, c("DelinquencyCategory","MonthsDq", "LastPaymentReceivedDate", "PaymentType", "TimesDq", "ServicerIdentifier", "StateCode", 
                                                 "LtvRatioPercentCurrent", "CurrentBalance", "NoteRatePercent", "NoteAmount", "Age", 
                                                 "PropertyValuationEffectiveDate","CreditScoreValueRecent"))
collateralLoans.h2o <- as.h2o(collateralLoans.gbm)

sample.training.h2o <- h2o.splitFrame( data = collateralLoans.h2o, ratios = 0.75)
collateralLoans.training.gbm <- sample.training.h2o[[1]]
collateralLoans.validation.gbm <- sample.training.h2o[[2]]

response.variable <- "DelinquencyCategory"
predictor.variables <- setdiff(names(collateralLoans.training.gbm), response.variable)
predictor.variables

gbm.parameters <- list(
                   learn_rate = seq(0.1, 0.3, 0.01),
                   max_depth = seq(2, 7, 1),
                   sample_rate = seq(0.9, 1.0, 0.05),  
                   col_sample_rate = seq(0.1, 1.0, 0.1))

search.criteria <- list(strategy = "RandomDiscrete", max_runtime_secs = 100)

gbm.grid <- h2o.grid("gbm",
                     x = predictor.variables,
                     y = response.variable,
                     grid_id = "gbm_grid",
                     training_frame = collateralLoans.training.gbm,
                     validation_frame = collateralLoans.validation.gbm,
                     ntrees = 700,
                     hyper_params = gbm.parameters,
                     distribution="multinomial",
                     stopping_rounds = 5, 
                     stopping_tolerance = 1e-4, 
                     stopping_metric = "MSE",
                     score_each_iteration = T,
                     search_criteria = search.criteria,
                     nfolds=5,
                     keep_cross_validation_predictions = TRUE)

#summary(gbm.grid, show_stack_traces = TRUE)

gbm.performance <- h2o.getGrid(grid_id = "gbm_grid", sort_by = "MSE", decreasing = FALSE)
gbm.performance

# Grab the model_id for the top GBM model, chosen by validation MSE
gbm.best.model <- h2o.getModel(gbm.performance@model_ids[[1]])

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
gbm.prediction.bestmodel <- h2o.predict(gbm.best.model, newdata = collateralLoans.validation.gbm)
gbm.prediction.bestmodel

gbm.performance.bestmodel <- h2o.performance(gbm.best.model, newdata = collateralLoans.validation.gbm)
gbm.performance.bestmodel

# @@@@@@@@@@@@@@@@@@ 3. Multinomial Logistic Regression 

collateralLoans.fit.logistic <- multinom(formula = (DelinquencyCategory~ MonthsDq + 
                                                      LastPaymentReceivedDate+PaymentType + 
                                                      TimesDq + ServicerIdentifier + StateCode + 
                                                      LtvRatioPercentCurrent + CurrentBalance +
                                                      NoteRatePercent + NoteAmount + Age + 
                                                      PropertyValuationEffectiveDate +
                                                      CreditScoreValueRecent),data=collateralLoans.training)

summary(collateralLoans.fit.logistic)

prob.predicted <- predict(collateralLoans.fit.logistic, type = "class", newdata = collateralLoans.validation)
prob.predicted

confusionMatrix.logistic <- table(prob.predicted,collateralLoans.validation$DelinquencyCategory)
confusionMatrix.logistic

accuracy.rate.logistic <- sum(diag(confusionMatrix.logistic)) / sum(confusionMatrix.logistic)
accuracy.rate.logistic * 100

misclassification.rate.logistic <- 100 - (accuracy.rate.logistic * 100)
misclassification.rate.logistic


# @@@@@@@@@@@@@@@@@@ 4. Stepwise Regression (forward)

step.model.forward <- collateralLoans.fit.logistic %>% stepAIC(trace = FALSE,direction ="forward")
summary(step.model.forward)


prob.predicted.step.forward <- predict(step.model.forward, type = "class", newdata = collateralLoans.validation)
prob.predicted.step.forward

confusionMatrix.step.forward <- table(prob.predicted.step.forward,collateralLoans.validation$DelinquencyCategory)
confusionMatrix.step.forward

accuracy.rate.step.forward <- sum(diag(confusionMatrix.step.forward)) / sum(confusionMatrix.step.forward)
accuracy.rate.step.forward * 100

misclassification.rate.step.forward <- 100 - (accuracy.rate.step.forward * 100)
misclassification.rate.step.forward


# @@@@@@@@@@@@@@@@@ 5. Stepwise Regression (backward)
step.model.backward <- collateralLoans.fit.logistic %>% stepAIC(trace = FALSE,direction ="backward")
summary(step.model.backward)


prob.predicted.step.backward <- predict(step.model.backward, type = "class", newdata = collateralLoans.validation)
prob.predicted.step.backward

confusionMatrix.step.backward<- table(prob.predicted.step.backward,collateralLoans.validation$DelinquencyCategory)
confusionMatrix.step.backward

accuracy.rate.step.backward <- sum(diag(confusionMatrix.step.backward)) / sum(confusionMatrix.step.backward)
accuracy.rate.step.backward * 100

misclassification.rate.step.backward <- 100 - (accuracy.rate.step.backward * 100)
misclassification.rate.step.backward
