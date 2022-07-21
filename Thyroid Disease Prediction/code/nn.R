install.packages("ggplot2")
install.packages("FSelector")
#Using Corrplot
install.packages("corrplot")
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
df=read.csv('/home/khadija/Documents/uOttawa/5126/W5/asg2/diabetes.csv')
for(i in 1:ncol(df)) {       # for-loop over columns
  for(j in 1:nrow(df)){
    if(df[j, i] == "?"){
      df[j, i]=NaN
    }     
  }
}

df$Pregnancies=as.numeric(df$Pregnancies)
df$Glucose=as.numeric(df$Glucose)
df$BloodPressure=as.numeric(df$BloodPressure)
df$SkinThickness=as.numeric(df$SkinThickness)
df$Insulin=as.numeric(df$Insulin)
df$BMI=as.numeric(df$BMI)
df$DiabetesPedigreeFunction=as.numeric(df$DiabetesPedigreeFunction)
df$Age=as.numeric(df$Age)
df$Outcome=as.numeric(df$Outcome)
qplot(df$Pregnancies,
      geom="histogram",
      binwidth=5,  
      main="Histogram for Pregnancies", 
      xlab="Pregnancies",
      ylab="Count")

qplot(df$Glucose,
      geom="histogram",
      binwidth=5,  
      main="Histogram for Glucose", 
      xlab="Glucose",
      ylab="Count")

qplot(df$BloodPressure,
      geom="histogram",
      binwidth=5,  
      main="Histogram for BloodPressure", 
      xlab="BloodPressure",
      ylab="Count")

qplot(df$SkinThickness,
      geom="histogram",
      binwidth=5,  
      main="Histogram for SkinThickness", 
      xlab="SkinThickness",
      ylab="Count")

qplot(df$Insulin,
      geom="histogram",
      binwidth=5,  
      main="Histogram for Insulin", 
      xlab="Insulin",
      ylab="Count")

qplot(df$BMI,
      geom="histogram",
      binwidth=5,  
      main="Histogram for BMI", 
      xlab="BMI",
      ylab="Count")
qplot(df$DiabetesPedigreeFunction,
      geom="histogram",
      binwidth=5,  
      main="Histogram for DiabetesPedigreeFunction", 
      xlab="DiabetesPedigreeFunction",
      ylab="Count")
qplot(df$Age,
      geom="histogram",
      binwidth=5,  
      main="Histogram for Age", 
      xlab="Age",
      ylab="Count")

qplot(df$Outcome,
      geom="histogram",
      binwidth=5,  
      main="Histogram for Outcome", 
      xlab="Outcome",
      ylab="Count")
# fill nans
df$BloodPressure[is.na(df$BloodPressure)] = mean(df$BloodPressure,na.rm = TRUE)
df$BMI[is.na(df$BMI)] = mean(df$BMI,na.rm = TRUE)
df$SkinThickness[is.na(df$SkinThickness)] = mean(df$SkinThickness,na.rm = TRUE)

df$Age[is.na(df$Age)] = median(df$Age, na.rm = TRUE)
df$Pregnancies[is.na(df$Pregnancies)]  = median(df$Pregnancies, na.rm = TRUE)

# drop rows of nans
df=na.omit(df) 
# split data set
set.seed(42)
sample_split <- sample.split(Y = df$Outcom, SplitRatio = 0.75)
train_set <- subset(x = df, sample_split == TRUE)
test_set <- subset(x = df, sample_split == FALSE)
# scale data
max = apply(df , 2 , max)
min = apply(df, 2 , min)
scaled = as.data.frame(scale(df, center = min, scale = max - min))
# modeling
# fit neural network
set.seed(2)

# install library
install.packages("neuralnet")
# load library
library(neuralnet)
NN = neuralnet(Outcome ~ Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age,train_set, hidden = 2 , linear.output = T )

# plot neural network
plot(NN)
## Prediction using neural network

predict_testNN = compute(NN, test_set[])

predict_testNN = (predict_testNN$net.result * (max(df$Outcome) - min(df$Outcome))) + min(df$Outcome)

plot(test_set$Outcome, predict_testNN, col='blue', pch=16, ylab = "predicted outcome NN", xlab = "real outcom")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
(sum((test_set$Outcome - predict_testNN)^2) / nrow(test_set)) ^ 0.5
###############################################################################3
NN = neuralnet(Outcome ~ Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age,train_set, hidden =c(5,2) ,act.fct = "logistic",linear.output = T )

# plot neural network
plot(NN)
## Prediction using neural network

predict_testNN = compute(NN, test_set[])

predict_testNN = (predict_testNN$net.result * (max(df$Outcome) - min(df$Outcome))) + min(df$Outcome)

plot(test_set$Outcome, predict_testNN, col='blue', pch=16, ylab = "predicted outcome NN", xlab = "real outcom")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
(sum((test_set$Outcome - predict_testNN)^2) / nrow(test_set)) ^ 0.5
###############################
NN = neuralnet(Outcome ~ Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age,train_set, hidden =c(5,3,2),learningrate.factor = list(minus = 1,threshold = 0.05,
                                                                                                                                                                       plus = 1.2),linear.output = T )

# plot neural network
plot(NN)
## Prediction using neural network

predict_testNN = compute(NN, test_set[])

predict_testNN = (predict_testNN$net.result * (max(df$Outcome) - min(df$Outcome))) + min(df$Outcome)

plot(test_set$Outcome, predict_testNN, col='blue', pch=16, ylab = "predicted outcome NN", xlab = "real outcom")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
(sum((test_set$Outcome - predict_testNN)^2) / nrow(test_set)) ^ 0.5

