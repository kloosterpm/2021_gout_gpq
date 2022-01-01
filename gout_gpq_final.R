# install.packages("haven")
# install.packages("caret")
# install.packages("e1071")
# install.packages("MASS")
# install.packages("binom")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("summarytools")
# install.packages("itertools")
# install.packages("randomForest")
# install.packages("missForest")
# install.packages("glmnet")

library(haven)
library(e1071)
library(MASS)
library(caret)
library(binom)
library(tidyverse)
library(dplyr)
library(summarytools)
library(itertools)
library(randomForest)
library(missForest)
library(glmnet)

data <- read_sav("spssdata.sav")
write.csv(data, "data.csv") # Save data frame as .csv
data <- read.csv("data.csv") # Read .csv data
data <- data[,-1] # Remove first column that is added by read.csv
data <- data.frame(data)

sapply(data, class) # Explore measurement level of variables

# Convert categorical variables to type factor
data$Male <- factor(data$Male)
data$Smoking <- factor(data$Smoking)
data$Alcohol <- factor(data$Alcohol)
data$OA <- factor(data$OA)
data$RA <- factor(data$RA)
data$FM <- factor(data$FM)
data$ULT <- factor(data$ULT)
data$Colchicine <- factor(data$Colchicine)
data$Paracetamol <- factor(data$Paracetamol)
data$NSAIDs <- factor(data$NSAIDs)
data$Opioid <- factor(data$Opioid)
data$Antiinflammatories <- factor(data$Antiinflammatories)
data$Antidepression <- factor(data$Antidepression)
data$antiepileptica <- factor(data$antiepileptica)
data$PD_cat <- factor(data$PD_cat)

sapply(data, class)

# Recode dummy-variable antinflammatories, so that Prednison and Dexamethason are 
# coded as one category (1) instead of two different categories
data$Antiinflammatories[which(data$Antiinflammatories == 2)] <- 1
data$Antiinflammatories <- factor(data$Antiinflammatories)

## Alternative: code "colcichine" or "NSAIDs" or "corticosteroids" as 1, versus none of them as 0

# create object that is either T or F, depending on whether the person received one of these treatments
#tmp <- data$NSAIDs == 1 | data$Colchicine == 1 | data$Antiinflammatories == 1 | data$Antiinflammatories == 2

#data$Antiinflammatories[tmp] <- 1 # Fill all rows that are TRUE in tmp with 1
#data$Antiinflammatories <- factor(data$Antiinflammatories) #Re-level factor

#data <- data[,-which(colnames(data) == "NSAIDs")] #Remove variable NSAIDs
#data <- data[,-which(colnames(data) == "Colchicine")] # Remove variable Colchicine

#table(data$Antiinflammatories) # Check distribution of the final variable

# Explore missing data
summary(data)
# Total dataset
table(is.na(data))
# Per variable
sapply(data, function(x) sum(is.na(x)))
colMeans(is.na(data))
# Per patient
rowSums(is.na(data))
rowMeans(is.na(data))
# Number of patients with no missing data
sum(complete.cases(data))

# Compute GPQ categories
data$GPQ_cat[data$GPQ_total > 10] <- 1
data$GPQ_cat[data$GPQ_total <= 10] <- 0
data$GPQ_cat <- factor(data$GPQ_cat)
sapply(data, class)

# Compute proportion of patients with GPQ >10
table(data$GPQ_cat, useNA = "ifany")
prop.table(table(data$GPQ_cat, useNA = "ifany"))
summarytools::freq(data$GPQ_cat)

# Calculate binomial CI around the proportion
binom.confint(x = 20, n = 97) # use exact (5) as method

# Nonparametric missing value imputation on mixed-type data:
set.seed(123)

data_imp <- missForest(data, verbose = TRUE) # missForest imputation using standard settings
data_imp$OOBerror
data_imp$error

data <- data_imp$ximp # Store imputed data into new object

summary(data)
str(data)
sum(complete.cases(data)) # Number of patients with no missing data

# Remove GPQ_total from data set (as this is the variable the response variable was based on)
data <- data[,-which(colnames(data) == "GPQ_total")]

# Remove the following variables because of lack of variance
data <- data[,-which(colnames(data) == "antiepileptica")]
data <- data[,-which(colnames(data) == "Antidepression")]

data <- data[,-which(colnames(data) == "PD_now")]
data <- data[,-which(colnames(data) == "PD_strongest")]
data <- data[,-which(colnames(data) == "PD_cat")]
data <- data[,-which(colnames(data) == "PD_total")]

# Rename levels of the response variable
levels(data$GPQ_cat) <- c("NGPijn","Pijnovergevoeligheid")

#####################
### ACTUAL MODELS ###
#####################

# Univariable logistic models
uni_logFit <- glm(GPQ_cat ~ Male, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ Age, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ Smoking, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ Alcohol, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ OA, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ RA, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ FM, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ attacks, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ ULT, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ Colchicine, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ Paracetamol, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ NSAIDs, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ Antiinflammatories, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ PD_average, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ HAQ_DI, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ SF12_MHC, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ SF12_PCS, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ SRPQ_dif, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

uni_logFit <- glm(GPQ_cat ~ SRPQ_sat, data=data, family = "binomial")
summary(uni_logFit)
round(exp(uni_logFit$coefficients),3); round(exp(confint(uni_logFit)),3)

########################
### LASSO REGRESSION ###
########################
x <- model.matrix(GPQ_cat ~ ., data = data)[,-1]

y <- data[,"GPQ_cat"]
y <- as.numeric(y)

# LASSO on the complete data
cv.lambda.lasso <- cv.glmnet(x, y, alpha=1, family = "binomial")

plot(cv.lambda.lasso)
cv.lambda.lasso
plot(cv.lambda.lasso$glmnet.fit, "lambda", label=T)

l.lasso.min <- cv.lambda.lasso$lambda.min
lasso.model <- glmnet(x=x, y=y, alpha=1, lambda = l.lasso.min)
lasso.model$beta

select_lasso <- c("Age","FM", "ULT", "Antiinflammatories", "PD_average", "SF12_MHC", "SRPQ_dif")

data_reduced <- data[,names(data) %in% select_lasso]
data_reduced$GPQ_cat <- data$GPQ_cat

### Use the reduced data set to conduct logistic regression (using caret package and cross-validation)
trControl = trainControl(method="cv", number=10, classProbs = TRUE, verboseIter = TRUE, savePredictions = TRUE)

logFit <- train(GPQ_cat ~., data=data_reduced, 
                trControl = trControl,
                method="glm", family="binomial")

summary(logFit)
confusionMatrix(logFit)

# Calculate ORs and confidence intervals
logFit2 <- glm(GPQ_cat ~., data=data_reduced, family = "binomial")
summary(logFit2)

round(exp(logFit2$coefficients),3) # ORs
round(exp(confint(logFit2)),3) # Confidence intervals





