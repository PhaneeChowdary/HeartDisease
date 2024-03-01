#Load the dependencies
library(readr)
library(rvest)
library(caTools)
library(stringr)
library(tidyverse)
library(dplyr)
library(party)
library(rpart)
library(rpart.plot)
library(class)

#Read CSV file and view
df = read_csv("heart.csv", show_col_types = FALSE)
View(df)
dim(df)
glimpse(df)


#First 6 records and last 6 records
head(df)
tail(df)


#Summarize the dataset
summary(df)



#Age :  Age of the patient
#Sex :  Gender of the patient
#exang: exercise induced angina (1 = yes; 0 = no)
#ca  :  number of major vessels (0-3) aorta. pulmonary artery. pulmonary veins.
#cp  :  Chest Pain type chest pain type
#       Value 1: typical angina
#       Value 2: atypical angina
#       Value 3: non-anginal pain
#       Value 4: asymptomatic
#trtbps: resting blood pressure (in mm Hg)
#chol : cholestrol in mg/dl fetched via BMI sensor
#fbs :  (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)
#rest_ecg : resting electrocardiographic results
#Value 0: normal
#Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)
#Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria
#thalach: maximum heart rate achieved
#target : 0= less chance of heart attack 
#         1= more chance of heart attack


#Check for duplicate data in dataset and remove if any
duplicated(df)
sum(duplicated(df))
uniq_df = distinct(df)
dim(uniq_df)


#Omit NAN value
uniq_df = na.omit(uniq_df)
dim(uniq_df)


#Visualizations
par(mfrow=c(3,2))
hist(uniq_df$age, xlab="Age", col="green")
hist(uniq_df$cp, xlab="Chest Pain", col="green")
hist(uniq_df$trestbps, xlab="Resting Blood Pressure", col="blue")
hist(uniq_df$chol, xlab="Cholestrol", col="blue")
hist(uniq_df$thalach, xlab="Max Heart rate", col="grey")

#Train and Test data
target_data = sample.split(uniq_df$target, SplitRatio=0.80)

train_data = subset(uniq_df, target_data==TRUE)
View(train_data)

test_data = subset(uniq_df, target_data==FALSE)
View(test_data)


#Model Building
tree = ctree(target~., train_data)
plot(tree)
fit = rpart(target~., data=train_data, method="class")
rpart.plot(fit)


#Predictions
preds = predict(fit, test_data, type="class")
table(preds)
table(train_data$target)

#Accuracy
dtc_error = sqrt((mean(preds != test_data$target))^2)
print(paste('Accuracy =', 1-dtc_error))

#KNN Classifier
knn_model = knn(train_data, test_data, cl = train_data$target, k = 15)
cm = table(test_data$target, knn_model)
cm

#Accuracy
error = sqrt((mean(knn_model != test_data$target)^2))
print(paste('Accuracy =', 1-error))
