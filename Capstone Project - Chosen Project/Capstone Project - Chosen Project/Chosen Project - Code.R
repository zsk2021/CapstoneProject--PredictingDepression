library("readxl")
library("dplyr")
library("ggplot2")
library('ggfortify')
library('corrplot')
library('stats')
library('purrr')
library('caTools')
library('e1071')
library('randomForest')
library('stringr')
library('class')
set.seed(1234)
# Here, I set the file to a path on my computer, but once this is saved to a 
# different computer, the path will need to be updated. The simplified data sheet 
# can also be downloaded from my git repository: 
# https://github.com/zsk2021/CapstoneProject--PredictingDepression
file_original <- "~/Desktop/HarvardX, EdX, Data Science/Capstone Project/Capstone Project - Chosen Project/Capstone Project - Chosen Project/Depression data-simple.xlsx"
temp_file <- read_excel(file_original)
# Converting the temp file into a transposed data frame.
file_df <- as.data.frame(t(temp_file))
# removing unnecessary rows/columns
data <- file_df[c(-2,-3,-4),c(-1,-3,-4,-5,-7)]

# Adding new column names for the molecules and the arms (groups):
data[1,1] <- 'Samples'
data[1,2] <- 'Arm'
colnames(data) <-data[1,]
# Remove first row from the data frame:
data <- data[-1,]
# By investigating the data, we can see that measurements come from two groups:
# Group 1 (control) and Group 2 (patients diagnosed with depression):
data %>% group_by(Arm) %>% summarise(n = n())

group_1 <- data %>% group_by(Arm) %>% filter(Arm == "Group 1 - Score 0")
# A data set without columns containing other characters than numbers is created:
group_1_truncated <- group_1[, c(-1, -2)]
# Same for group 2:
group_2 <- data %>% group_by(Arm) %>% filter(Arm == "Group 2 - Score 50")
group_2_truncated <- group_2[, c(-1, -2)]

# I used Shapiro-Wilk's method (http://www.sthda.com/english/wiki/normality-test-in-r)
# to get a value of the normality for each measured parameter. The null hypothesis for this test is 
# that “the sample distribution is normal”. So, if the p-value is >0.05, that implies that the 
# distribution of the data is not significantly different from the normal distribution. 
# In other words, if the p-value is >0.05 we can assume normality. 
# First, I loop through the truncated and transposed list and generate Shapiro-Wilk's test for 
# each column in the data set. I use the magicfor library to record p-values in a vector:  

library(magicfor)
# Group 1:
magic_for(print)
for (c in group_1_truncated){
  shap_test <- shapiro.test(as.numeric(c))
  output <- shap_test$p.value
  print(output)
}
# Saving the printed p-values as a vector:
pvalues_group_1 <- magic_result_as_vector()
# Binding vector to the original data, so the last row is the p-value from the 
# Shapiro-Wilk's test:
group_1_truncated_with_pvalues <- rbind(group_1_truncated, pvalues_group_1)
  
# Group 2:
magic_for(print)
for (c in group_2_truncated){
  shap_test <- shapiro.test(as.numeric(c))
  output <- shap_test$p.value
  print(output)
}
# Saving the printed p-values as a vector:
pvalues_group_2 <- magic_result_as_vector()
# Binding vector to the original data, so the last row is the p-value from the 
# Shapiro-Wilk's test:
group_2_truncated_with_pvalues <- rbind(group_2_truncated,pvalues_group_2)
# Remove magicalization:
magic_free()
group_1_truncated_with_pvalues %>% 
  summarise(Arm = 'Group 2', 
            nrow = dim(group_1_truncated_with_pvalues)[1], 
            ncol = dim(group_1_truncated_with_pvalues)[2])
group_2_truncated_with_pvalues %>% 
  summarise(Arm = 'Group 2', 
            nrow = dim(group_2_truncated_with_pvalues)[1], 
            ncol = dim(group_2_truncated_with_pvalues)[2])
# Now, that I have the p-values for the Shapiro-Wilk's test for the measured parameters from
# each arm, I transpose the data frames, so the p-values are in a separate column and the data 
# frame is in tidy format:
# Group 1 - transpose
group_1_tidy <- as.data.frame(t(group_1_truncated_with_pvalues))
# adding one extra row that will be used for the column names
group_1_tidy<- add_row(group_1_tidy, .before = 1)
# using sample names for column names
group_1_tidy[1,1:48] <- group_1$Samples
# adding name for extra column
group_1_tidy[1,49] <- "Shapiro-Wilk's p-values"
# use first row as column names
colnames(group_1_tidy) <- group_1_tidy[1,]
group_1_tidy <- group_1_tidy[-1,]
# The last column is the Shapiro-Wilk's p-values
group_1_tidy %>% summarise(Arm = 'Group 1', nrow = dim(group_1_tidy)[1], 
                           ncol = dim(group_1_tidy)[2])
  
# Group 2 -transpose
group_2_tidy <- as.data.frame(t(group_2_truncated_with_pvalues))
# adding on extra row that will be used for the column names
group_2_tidy<- add_row(group_2_tidy, .before = 1)
# using sample names for column names
group_2_tidy[1,1:49] <- group_2$Samples
# adding name for extra column
group_2_tidy[1,50] <- "Shapiro-Wilk's p-values"
# use first row as column names
colnames(group_2_tidy) <- group_2_tidy[1,]
group_2_tidy <- group_2_tidy[-1,]
# The last column is the Shapiro-Wilk's p-values
group_2_tidy %>% summarise(Arm = 'Group 2', nrow = dim(group_2_tidy)[1], 
                           ncol = dim(group_2_tidy)[2])

# I keep all measured parameters, where the p-value was >0.05:
group_1_tidy <- group_1_tidy %>% filter(`Shapiro-Wilk's p-values`>0.05)
group_2_tidy <- group_2_tidy %>% filter(`Shapiro-Wilk's p-values`>0.05)

# Adding row names as an extra column, so I can use semi_join:
group_1_tidy <- cbind(group_1_tidy, rownames = rownames(group_1_tidy))
group_2_tidy <- cbind(group_2_tidy, rownames = rownames(group_2_tidy))
# We should have one extra column in each data frame:
group_1_tidy %>% summarise(Arm = 'Group 1', nrow = dim(group_1_tidy)[1], 
                           ncol = dim(group_1_tidy)[2])
group_2_tidy %>% summarise(Arm = 'Group 2', nrow = dim(group_2_tidy)[1], 
                           ncol = dim(group_2_tidy)[2])
# Keep everything from Group 1 with a match in Group 2:
group_1_tidy <- semi_join(group_1_tidy, group_2_tidy, by = "rownames")
# Keep everything from Group 2 with a match in Group 1:
group_2_tidy <- semi_join(group_2_tidy, group_1_tidy, by = "rownames")

## *Two-Sample t-Test*  
# Transpose tidy data, so I can loop through the columns: Group 1
group_1_tidy_t <- as.data.frame(t(group_1_tidy))
# Removing last two rows with p-values and row names; these are not needed for the
# calculation of two-sample t-test.
group_1_tidy_t <- group_1_tidy_t[c(-49,-50),]
# Transpose tidy data, so I can loop through the columns: Group 2
group_2_tidy_t <- as.data.frame(t(group_2_tidy))
# Removing last two rows with p-values and row names; these are not needed for the
# calculation of two-sample t-test.
group_2_tidy_t <- group_2_tidy_t[c(-50,-51),]
# The two data set has 78 measured parameters and 48 and 49 samples, respectively:
group_1_tidy_t %>% summarise(Arm = 'Group 1', nrow = dim(group_1_tidy_t)[1], 
                             ncol = dim(group_1_tidy_t)[2])
group_2_tidy_t %>% summarise(Arm = 'Group 2', nrow = dim(group_2_tidy_t)[1], 
                             ncol = dim(group_2_tidy_t)[2])

# Two-sample t-test by looping through the columns:
magic_for(print)
for (j in seq(ncol(group_1_tidy_t))){
  testresults <- t.test(as.numeric(group_1_tidy_t[,j]), as.numeric(group_2_tidy_t[,j]))
  print(testresults$p.value)
}

# Saving p-values from the two-sample t-test into a data frame:
twosample_ttest <- magic_result_as_dataframe()
magic_free()
# Adding the names of the measured parameters to the p-values:
colnames(twosample_ttest)[1] <- 'rownames'
twosample_ttest$`rownames` <- colnames(group_1_tidy_t)
# Filtering out measured parameters that showed significant differences between
# the two groups: 
twosample_ttest_significant <- twosample_ttest %>% filter(`testresults$p.value` <= 0.05)
twosample_ttest_significant

## *Correlation Analysis*  
# Initially, I will subset the two data frames group_1_tidy
# and group_2_tidy to only consist of the 15 parameters of interest.
group_1_final <- semi_join(group_1_tidy, twosample_ttest_significant, by = 'rownames')
group_1_final <- group_1_final[,c(-49,-50)]

group_2_final <- semi_join(group_2_tidy, twosample_ttest_significant, by = 'rownames')
group_2_final <- group_2_final[,c(-50,-51)]

# Now, that I have the two data frames with the 15 measured parameters that showed approximately
# normal distribution and significant differences between the two groups, I will merge the 
# two arms, and will generate a new data frame with all of the subjects and the 15 measured parameters.
# I will use this data frame for my further work:
# First, I create a new column in both data frames, so I can merge these with 
# left_join()
group_1_final <- cbind(group_1_final, rownames = rownames(group_1_final))
group_2_final <- cbind(group_2_final, rownames = rownames(group_2_final))
# Merging the two data frames by rownames:
df <- left_join(group_1_final, group_2_final, by = "rownames")
# Adding rownames based on the rownames column
rownames(df) <- df$rownames
# Removing rownames column results in the final data set df:
df <- subset(df, select = -rownames)

# In the above section, I generated my data set with all 15 parameters and the entire 
# cohort. I now transpose this and will do a correlation analysis to see if any of 
# these parameters are correlated:
df_t <- as.data.frame(t(df))

plot(df_t$`stearic acid`, df_t$`heptadecanoic acid`)
# A correlation analysis between the two parameters shows a strong positive correlation
# with a value of 0.862:
cor.test(as.numeric(df_t$`stearic acid`), as.numeric(df_t$`heptadecanoic acid`))
# Here, I convert all df_t to numeric, so I can do a correlation analysis:
df_num <-as.data.frame(sapply(df_t, as.numeric))
# This also shows, that the only correlation is between stearic acid and heptadecanoic 
# acid:
cor_15_param <- as.data.frame(cor(df_num))
cor_15_param %>% filter(cor_15_param >= 0.7)

# Model Fitting
# K-means Clustering

wssplot <- function(data, nc=15, seed=1234){
                  wss <- (nrow(data)-1)*sum(apply(data,2,var))
                      for (i in 2:nc){
                set.seed(seed)
                    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
              plot(1:nc, wss, type="b", xlab="Number of Clusters",
                            ylab="Within groups sum of squares")
              wss
       }
wssplot(df_num)
# From the above plot, we can see that the optimum number of clusters 3 (the smallest 
# possible number, where the plot shows an elbow shape). So, we will apply the 
# cluster numbers 3 in our k-means cluster analysis.

KM_3 <- kmeans(df_num, 3)
print(KM_3)
autoplot(KM_3, df_num, frame = TRUE)
KM_3$centers
wssplot(df_num)
KM_2 <- kmeans(df_num, 2)
print(KM_2)
autoplot(KM_2, df_num, frame = TRUE)
KM_2$centers

# Support Vector Machine
# Creating a training and test set from the data frame df_num.
# First I add the arm to the data set, so the split can be done based on these features:
df_num_sp <- cbind(df_num, Arm = data$Arm)
index <- sample.split(df_num_sp$Arm, SplitRatio = .7)
SVM_tr <- subset(df_num_sp, index == TRUE)
SVM_val <- subset(df_num_sp, index == FALSE)
# Splitting the training set into further training and test sets:
index_train <- sample.split(SVM_tr$Arm, SplitRatio = .5)
SVM_train <- subset(SVM_tr, index_train == TRUE)
SVM_test <- subset(SVM_tr, index_train == FALSE)
# SVM model with linear kernel: raw values
svm_model_linear <- svm(as.factor(Arm)~
                          as.numeric(SVM_train$`stearic acid`) +
                          as.numeric(SVM_train$nicotinamide) +
                          as.numeric(SVM_train$`myo-inositol`) +
                          as.numeric(SVM_train$mannose) +
                          as.numeric(SVM_train$`heptadecanoic acid`) +
                          as.numeric(SVM_train$`glutamic acid`),
                        data = SVM_train, method = "C", kernel = "linear", 
                        gamma = 1, cost = 2)
# Getting the mean of the correctly predicted arm on the train data set:
predict_tr_linear <- predict(svm_model_linear, SVM_train)
classification_accuracy_SVM_tr <- mean(predict_tr_linear == SVM_train$Arm)
classification_accuracy_SVM_tr
# Getting the mean of the correctly predicted arm on the test data set:
predict_test_linear <- predict(svm_model_linear, SVM_test)
classification_accuracy_SVM_test <- mean(predict_test_linear == SVM_test$Arm)
classification_accuracy_SVM_test

# Creating a standardized training and test set from the data frame df_num:
df_num_st <- as_tibble(scale(df_num))
df_num_st <- cbind(df_num_st, Arm = data$Arm)
index_st <- sample.split(df_num_st$Arm, SplitRatio = .7)
SVM_tr_st <- subset(df_num_st, index == TRUE)
SVM_val_st <- subset(df_num_st, index == FALSE)
# Splitting the standardized training set into further training and test sets:
index_train_st <- sample.split(SVM_tr_st$Arm, SplitRatio = .5)
SVM_train_st <- subset(SVM_tr_st, index_train_st == TRUE)
SVM_test_st <- subset(SVM_tr_st, index_train_st == FALSE)
# SVM model with linear kernel: standardized data
svm_model_linear_st <- svm(as.factor(Arm)~
                          as.numeric(SVM_train_st$`stearic acid`) +
                          as.numeric(SVM_train_st$nicotinamide) +
                          as.numeric(SVM_train_st$mannose) +
                          as.numeric(SVM_train_st$`myo-inositol`) +
                          as.numeric(SVM_train_st$`heptadecanoic acid`) +
                          as.numeric(SVM_train_st$`glutamic acid`),
                        data = SVM_train_st, method = "C", kernel = "linear", 
                        gamma = 1, cost = 2)
# Getting the mean of the correctly predicted arm on the standardized train data set:
predict_tr_linear_st <- predict(svm_model_linear_st, SVM_train_st)
classification_accuracy_SVM_st_tr <- mean(predict_tr_linear_st == SVM_train_st$Arm)
classification_accuracy_SVM_st_tr
# Getting the mean of the correctly predicted arm on the standardized test data set:
predict_test_linear_st <- predict(svm_model_linear_st, SVM_test_st)
classification_accuracy_SVM_st_test <- mean(predict_test_linear_st == SVM_test_st$Arm)
classification_accuracy_SVM_st_test

# k-NN model
# Re-initializing the scaled data set used for the k-means calculation above
df_num_st <- as_tibble(scale(df_num))
df_num_st <- cbind(df_num_st, Arm = data$Arm)
df_num_st <- df_num_st %>% select(1, 6, 7, 8, 10, 12, 16)
index_st <- sample.split(df_num_st$Arm, SplitRatio = .7)
knn_tr <- subset(df_num_st, index == TRUE)
knn_valid <- subset(df_num_st, index == FALSE)
index_knn_tr <- sample.split(knn_tr$Arm, SplitRatio = .5)
knn_train <- subset(knn_tr, index_knn_tr == TRUE)
knn_test <- subset(knn_tr, index_knn_tr == FALSE)
pred_knn <- knn(knn_train[,-7], knn_test[,-7], knn_train[,7], k = 21)
# Next we validate the predicted labels with the actual labels:
CFM_knn <- table(pred_knn, knn_test[,7])
CFM_knn
classification_accuracy_knn <- sum(diag(CFM_knn))/sum(CFM_knn)
classification_accuracy_knn

# Random Forest Model
# Binding the Arm variables to the df_num data set:
df_num_RMF <- cbind(df_num, Arm = data$Arm)
# Selecting parameters highlighted by the k-nearest model:
df_num_RMF <- df_num_RMF %>% select(1, 6, 7, 8, 10, 12, 16)
# Converting the parameter names, so these do not consist of space or any illegal characters:
df_colnames <- sub(' ', '_', colnames(df_num_RMF))
df_colnames <- sub('-', '_', df_colnames)
# Adding back the converted names to the column headers.
colnames(df_num_RMF) <- df_colnames
# Creating a training and test set from the data frame df_num_RMF:
index_RMF <- sample.split(df_num_RMF$Arm, SplitRatio = .7)
RMF_tr <- subset(df_num_RMF, index == TRUE)
RMF_val <- subset(df_num_RMF, index == FALSE)
# Splitting the train set into further train and test sets:
index_train_RMF <- sample.split(RMF_tr$Arm, SplitRatio = .5)
RMF_train <- subset(RMF_tr, index_train == TRUE)
RMF_test <- subset(RMF_tr, index_train == FALSE)
RMF <- randomForest(as.factor(Arm)~nicotinamide + 
                      stearic_acid + 
                      mannose + 
                      heptadecanoic_acid +
                      glutamic_acid + 
                      myo_inositol, 
                    data = RMF_train)

# Now that we have built the RMF model, we use the predict function to get the
# predicted values for the data set:
predict_RMF_tr <- predict(RMF, RMF_train)
# Adding the predicted arms to the data set:
RMF_train$predict_RMF_tr <- predict_RMF_tr
# Compare the predictions and the actual values to see the accuracy of the model
# using the table() function (building a confusion matrix):
CFM_RMF_tr <- table(RMF_train$Arm, RMF_train$predict_RMF_tr)
CFM_RMF_tr
# Calculating the accuracy of the testing data can be measured by adding together 
# all the diagonal values, and dividing with the sum of all values:
classification_accuracy_RMF_tr <- sum(diag(CFM_RMF_tr)/sum(CFM_RMF_tr))
classification_accuracy_RMF_tr
# Testing the RMF model:
predict_RMF_test <- predict(RMF, RMF_test)
RMF_test$predict_RMF_test <- predict_RMF_test
CFM_RMF_test <- table(RMF_test$Arm, RMF_test$predict_RMF_test)
CFM_RMF_test
classification_accuracy_RMF_test <- sum(diag(CFM_RMF_test)/sum(CFM_RMF_test))
classification_accuracy_RMF_test

# Validation
# SVM

# Non-standardized Values - Linear
predict_valid_linear <- predict(svm_model_linear, SVM_val)
classification_accuracy_SVM_val <- mean(predict_valid_linear == SVM_val$Arm)
classification_accuracy_SVM_val
# Standardized Values - Linear
predict_valid_linear_st <- predict(svm_model_linear_st, SVM_val_st)
classification_accuracy_SVM_st_val <- mean(predict_valid_linear_st == SVM_val_st$Arm)
classification_accuracy_SVM_st_val

# k-NN
pred_knn_valid <- knn(knn_train[,-7], knn_valid[,-7], knn_train[,7], k = 15)
# Next we validate the predicted labels with the actual labels:
CFM_knn_valid <- table(pred_knn_valid, knn_valid[,7])
CFM_knn_valid
classification_accuracy_knn_valid <- sum(diag(CFM_knn_valid))/sum(CFM_knn_valid)
classification_accuracy_knn_valid

predict_valid_RMF <- predict(RMF, RMF_val)
RMF_val$predict_valid_RMF <- predict_valid_RMF
CFM_RMF_val <- table(RMF_val$Arm, RMF_val$predict_valid_RMF)
CFM_RMF_val
classification_accuracy_RMF_val <- sum(diag(CFM_RMF_val)/sum(CFM_RMF_val))
classification_accuracy_RMF_val

model_results_tibble <- tibble(Models = c("SVM Linear", 
                                          "SVM Standardized Linear",
                                          "RMF", "k-NN"),
                               TrainFit = c(classification_accuracy_SVM_tr,
                                            classification_accuracy_SVM_st_tr,
                                            classification_accuracy_RMF_tr,
                                            '-'),
                               TestFit = c(classification_accuracy_SVM_test,
                                           classification_accuracy_SVM_st_test,
                                           classification_accuracy_RMF_test,
                                           classification_accuracy_knn),
                               Validation = c(classification_accuracy_SVM_val,
                                              classification_accuracy_SVM_st_val,
                                              classification_accuracy_RMF_val,
                                              classification_accuracy_knn_valid)) %>% 
  mutate(TestFit = sprintf("%0.4f", TestFit))
  
model_results_tibble


