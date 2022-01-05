# Read in packages
set.seed(123)
library(caret)
library(ROCR)
library(tidyverse)
library(janitor)
library(forcats)
library(mice)
library(imputeTS)

# Read in dataset and convert target to factor
medicalData <- read.csv("medical_data/medicalData.csv")
medicalData$readmitted[medicalData$readmitted == "NO" | medicalData$readmitted == ">30"] <- 0
medicalData$readmitted[medicalData$readmitted == "<30"] <- 1
medicalData$readmitted <- factor(medicalData$readmitted)
medicalData <- subset(medicalData[, -c(3, 11:12)])

# FEATURE ENGINEERING
#-------------------------------------------------------------------------------

# Set up MICE for data imputation
init = mice(medicalData, maxit=0) 
meth = init$method
predM = init$predictorMatrix

# Convert colunm to numerical
medicalData$diag_1 <- as.numeric(medicalData$diag_1)
medicalData$diag_2 <- as.numeric(medicalData$diag_2)
medicalData$diag_3 <- as.numeric(medicalData$diag_3)
medicalData$diag_1 <- na_replace(medicalData$diag_1, fill = 0)
medicalData$diag_2 <- na_replace(medicalData$diag_2, fill = 0)
medicalData$diag_3 <- na_replace(medicalData$diag_3, fill = 0)

# Grouping diagnosis codes by chapter per ICD-9
medicalData$diag_1[medicalData$diag_1 >= 1 & medicalData$diag_1 <= 139] <- "001-139"
medicalData$diag_1[medicalData$diag_1 >= 140 & medicalData$diag_1 <= 239] <- "140-239"
medicalData$diag_1[medicalData$diag_1 >= 240 & medicalData$diag_1 <= 279] <- "240-279"
medicalData$diag_1[medicalData$diag_1 >= 280 & medicalData$diag_1 <= 289] <- "280-289"
medicalData$diag_1[medicalData$diag_1 >= 290 & medicalData$diag_1 <= 319] <- "290-319"
medicalData$diag_1[medicalData$diag_1 >= 320 & medicalData$diag_1 <= 389] <- "320-389"
medicalData$diag_1[medicalData$diag_1 >= 390 & medicalData$diag_1 <= 459] <- "390-459"
medicalData$diag_1[medicalData$diag_1 >= 460 & medicalData$diag_1 <= 519] <- "460-519"
medicalData$diag_1[medicalData$diag_1 >= 520 & medicalData$diag_1 <= 579] <- "520-579"
medicalData$diag_1[medicalData$diag_1 >= 580 & medicalData$diag_1 <= 629] <- "580-629"
medicalData$diag_1[medicalData$diag_1 >= 460 & medicalData$diag_1 <= 519] <- "460-519"
medicalData$diag_1[medicalData$diag_1 >= 630 & medicalData$diag_1 <= 679] <- "630-679"
medicalData$diag_1[medicalData$diag_1 >= 680 & medicalData$diag_1 <= 709] <- "680-709"
medicalData$diag_1[medicalData$diag_1 >= 710 & medicalData$diag_1 <= 739] <- "710-739"
medicalData$diag_1[medicalData$diag_1 >= 740 & medicalData$diag_1 <= 759] <- "740-759"
medicalData$diag_1[medicalData$diag_1 >= 760 & medicalData$diag_1 <= 779] <- "760-779"
medicalData$diag_1[medicalData$diag_1 >= 780 & medicalData$diag_1 <= 799] <- "780-799"
medicalData$diag_1[medicalData$diag_1 >= 800 & medicalData$diag_1 <= 999] <- "800-999"

medicalData$diag_2[medicalData$diag_2 >= 1 & medicalData$diag_2 <= 139] <- "001-139"
medicalData$diag_2[medicalData$diag_2 >= 140 & medicalData$diag_2 <= 239] <- "140-239"
medicalData$diag_2[medicalData$diag_2 >= 240 & medicalData$diag_2 <= 279] <- "240-279"
medicalData$diag_2[medicalData$diag_2 >= 280 & medicalData$diag_2 <= 289] <- "280-289"
medicalData$diag_2[medicalData$diag_2 >= 290 & medicalData$diag_2 <= 319] <- "290-319"
medicalData$diag_2[medicalData$diag_2 >= 320 & medicalData$diag_2 <= 389] <- "320-389"
medicalData$diag_2[medicalData$diag_2 >= 390 & medicalData$diag_2 <= 459] <- "390-459"
medicalData$diag_2[medicalData$diag_2 >= 460 & medicalData$diag_2 <= 519] <- "460-519"
medicalData$diag_2[medicalData$diag_2 >= 520 & medicalData$diag_2 <= 579] <- "520-579"
medicalData$diag_2[medicalData$diag_2 >= 580 & medicalData$diag_2 <= 629] <- "580-629"
medicalData$diag_2[medicalData$diag_2 >= 460 & medicalData$diag_2 <= 519] <- "460-519"
medicalData$diag_2[medicalData$diag_2 >= 630 & medicalData$diag_2 <= 679] <- "630-679"
medicalData$diag_2[medicalData$diag_2 >= 680 & medicalData$diag_2 <= 709] <- "680-709"
medicalData$diag_2[medicalData$diag_2 >= 710 & medicalData$diag_2 <= 739] <- "710-739"
medicalData$diag_2[medicalData$diag_2 >= 740 & medicalData$diag_2 <= 759] <- "740-759"
medicalData$diag_2[medicalData$diag_2 >= 760 & medicalData$diag_2 <= 779] <- "760-779"
medicalData$diag_2[medicalData$diag_2 >= 780 & medicalData$diag_2 <= 799] <- "780-799"
medicalData$diag_2[medicalData$diag_2 >= 800 & medicalData$diag_2 <= 999] <- "800-999"

medicalData$diag_3[medicalData$diag_3 >= 1 & medicalData$diag_3 <= 139] <- "001-139"
medicalData$diag_3[medicalData$diag_3 >= 140 & medicalData$diag_3 <= 239] <- "140-239"
medicalData$diag_3[medicalData$diag_3 >= 240 & medicalData$diag_3 <= 279] <- "240-279"
medicalData$diag_3[medicalData$diag_3 >= 280 & medicalData$diag_3 <= 289] <- "280-289"
medicalData$diag_3[medicalData$diag_3 >= 290 & medicalData$diag_3 <= 319] <- "290-319"
medicalData$diag_3[medicalData$diag_3 >= 320 & medicalData$diag_3 <= 389] <- "320-389"
medicalData$diag_3[medicalData$diag_3 >= 390 & medicalData$diag_3 <= 459] <- "390-459"
medicalData$diag_3[medicalData$diag_3 >= 460 & medicalData$diag_3 <= 519] <- "460-519"
medicalData$diag_3[medicalData$diag_3 >= 520 & medicalData$diag_3 <= 579] <- "520-579"
medicalData$diag_3[medicalData$diag_3 >= 580 & medicalData$diag_3 <= 629] <- "580-629"
medicalData$diag_3[medicalData$diag_3 >= 460 & medicalData$diag_3 <= 519] <- "460-519"
medicalData$diag_3[medicalData$diag_3 >= 630 & medicalData$diag_3 <= 679] <- "630-679"
medicalData$diag_3[medicalData$diag_3 >= 680 & medicalData$diag_3 <= 709] <- "680-709"
medicalData$diag_3[medicalData$diag_3 >= 710 & medicalData$diag_3 <= 739] <- "710-739"
medicalData$diag_3[medicalData$diag_3 >= 740 & medicalData$diag_3 <= 759] <- "740-759"
medicalData$diag_3[medicalData$diag_3 >= 760 & medicalData$diag_3 <= 779] <- "760-779"
medicalData$diag_3[medicalData$diag_3 >= 780 & medicalData$diag_3 <= 799] <- "780-799"
medicalData$diag_3[medicalData$diag_3 >= 800 & medicalData$diag_3 <= 999] <- "800-999"

# Impute missing weight data using MICE
medicalData <- medicalData %>%
  mutate(
    weight = as.factor(weight)
  )
meth[c("weight")]="polyreg"
set.seed(123)
imputed = mice(medicalData, method=meth, predictorMatrix=predM, m = 5)
medicalData <- complete(imputed)

# Calculating the number of total encounters
medicalData$number_encounters <- medicalData$number_outpatient + medicalData$number_emergency + medicalData$number_inpatient

# Breaking down medications and procedures administered per time in the hospital
medicalData$lab_procedures_per_day <- medicalData$num_lab_procedures / medicalData$time_in_hospital
medicalData$medications_per_day <- medicalData$num_medications / medicalData$time_in_hospital
medicalData$procedures_per_day <- medicalData$num_procedures / medicalData$time_in_hospital

# Converting categorical variables to factors
medicalData$diag_1 <- factor(medicalData$diag_1)
medicalData$diag_2 <- factor(medicalData$diag_2)
medicalData$diag_3 <- factor(medicalData$diag_3)
medicalData$admission_type_id <- factor(medicalData$admission_type_id)
medicalData$discharge_disposition_id <- factor(medicalData$discharge_disposition_id)
medicalData$admission_source_id <- factor(medicalData$admission_source_id)
medicalData$age <- factor(medicalData$age)
medicalData$weight <- factor(medicalData$weight)

# Split the data into training and (held-out) test sets
training_ind <- createDataPartition(medicalData$readmitted, p = 0.25,
                                    list = FALSE,
                                    times = 1)
training_set <- medicalData[training_ind, ]
test_set <- medicalData[-training_ind, ]

# One hot encoding for categorical variables 
onehot_encoder <- dummyVars(~ diag_1 + diag_2 + diag_3 + admission_type_id + discharge_disposition_id + admission_source_id + age + weight,
                            medicalData[, c("diag_1", "diag_2", "diag_3", "admission_type_id", "discharge_disposition_id", "admission_source_id", "age", "weight")],
                            levelsOnly = FALSE) 
onehot_enc_training <- predict(onehot_encoder,
                               training_set[, c("diag_1", "diag_2", "diag_3", "admission_type_id", "discharge_disposition_id", "admission_source_id", "age", "weight")]) 
training_set <- cbind(training_set, onehot_enc_training)

onehot_enc_test <- predict(onehot_encoder, 
                           test_set[, c("diag_1", "diag_2", "diag_3", "admission_type_id", "discharge_disposition_id", "admission_source_id", "age", "weight")]) 
test_set <- cbind(test_set, onehot_enc_test)

# Normalize and scale numerical values
test_set[, c(1:2, 9:15, 19, 48:51)] <- scale(test_set[, c(1:2, 9:15, 19, 48:51)],
                          center = apply(training_set[, c(1:2, 9:15, 19, 48:51)], 2, mean),
                          scale = apply(training_set[, c(1:2, 9:15, 19, 48:51)], 2, sd)) 
training_set[, c(1:2, 9:15, 19, 48:51)] <- scale(training_set[, c(1:2, 9:15, 19, 48:51)])

# Renaming levels of the target variable
levels(training_set$readmitted)[levels(training_set$readmitted)==0] <- "negative" 
levels(training_set$readmitted)[levels(training_set$readmitted)==1] <- "positive"
levels(test_set$readmitted)[levels(test_set$readmitted)==0] <- "negative" 
levels(test_set$readmitted)[levels(test_set$readmitted)==1] <- "positive"

modelLookup("rf")

recommended_mtry <- floor(sqrt(ncol(training_set[, -1*c(1:2, 4:8, 16:18, 47)])))
rfGrid <- expand.grid(mtry = c(recommended_mtry-2, recommended_mtry, recommended_mtry+2))

rfControl <- trainControl(method = "oob",
                          classProbs = TRUE)

rf_onehot_model <- train(x = training_set[, -1*c(1:2, 4:8, 16:18, 47)], y = training_set[, 47], 
                         method = "rf",
                         tuneGrid = rfGrid,
                         trControl = rfControl,
                         importance = TRUE,
                         trace = FALSE) 

test_set$prediction_onehot <- predict(rf_onehot_model, 
                                      newdata = test_set[, -1*c(1:2, 4:8, 16:18, 47)])

class_probabilities <- predict(rf_onehot_model,
                               newdata = test_set[, -1*c(1:2, 4:8, 16:18, 47)],
                               type = "prob")
test_set$class_probabilities_onehot <- class_probabilities$positive

rocr_pred <- prediction(test_set$class_probabilities_onehot, test_set$readmitted) 
rocr_roc <- performance(rocr_pred, measure = "tpr", x.measure = "fpr")
plot(rocr_roc,
     colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.5, 1),
     lwd = 2)
abline(a = 0, b = 1)

rocr_auc <- performance(rocr_pred, measure = "auc") 
auc <- rocr_auc@y.values[[1]]
auc

calibration_curve <- calibration(readmitted ~ class_probabilities_onehot, 
                                 data = test_set,
                                 class = 1)
plot(calibration_curve)

rf_varImp <- varImp(rf_onehot_model, type = 2)
plot(rf_varImp, top = 25)

  
