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
df=read.csv('/home/khadija/Documents/uOttawa/5126/W5/asg2/hypothyroid.csv')
# https://statisticsglobe.com/loop-through-data-frame-columns-rows-in-r/
for(i in 1:ncol(df)) {       # for-loop over columns
  for(j in 1:nrow(df)){
    if(df[j, i] == "?"){
      df[j, i]=NaN
    }     
  }
}
# convert numeric colum from factor to numeric data type
df$age=as.numeric(df$age)
df$TSH=as.numeric(df$TSH)
df$T3=as.numeric(df$T3)
df$TT4=as.numeric(df$TT4)
df$T4U=as.numeric(df$T4U)
df$FTI=as.numeric(df$FTI)
qplot(df$age,
      geom="histogram",
      binwidth=5,  
      main="Histogram for Age", 
      xlab="Age",
      ylab="Count")

qplot(df$TSH,
      geom="histogram",
      binwidth=5,  
      main="Histogram for TSH", 
      xlab="TSH",
      ylab="Count")
qplot(df$T3,
      geom="histogram",
      binwidth=5,  
      main="Histogram for T3", 
      xlab="T3",
      ylab="Count")
qplot(df$TT4,
      geom="histogram",
      binwidth=5,  
      main="Histogram for TT4", 
      xlab="TT4",
      ylab="Count")
qplot(df$T4U,
      geom="histogram",
      binwidth=5,  
      main="Histogram for T4U", 
      xlab="T4U",
      ylab="Count")
qplot(df$FTI,
      geom="histogram",
      binwidth=5,  
      main="Histogram for FTI", 
      xlab="FTI",
      ylab="Count")
# fill nans
df$age[is.na(df$age)] = mean(df$age,na.rm = TRUE)
df$T4U[is.na(df$T4U)] = mean(df$T4U,na.rm = TRUE)
df$T4U[is.na(df$T4U)] = median(df$T4U, na.rm = TRUE)
df$T3[is.na(df$T3)]  = median(df$T3, na.rm = TRUE)

# drop TBG ( a col of nans)
df = subset(df, select = -c(TBG) )
# drop rows of nans
df=na.omit(df) 
str(df)
# convert numerical features to numerical data type
df$age=as.numeric(df$age)
df$TSH=as.numeric(df$TSH)
df$T3=as.numeric(df$T3)
df$TT4=as.numeric(df$TT4)
df$T4U=as.numeric(df$T4U)
df$FTI=as.numeric(df$FTI)

# categorical insights
ggplot(data=df, aes(x=sex, y=age)) +
  geom_bar(stat="identity")
#######################################################3

library('FSelector')
gain.ratio(Class~., df)
#############################################
#https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
library(dplyr)

dt_df=select(df,on_thyroxine, pregnant, thyroid_surgery,query_hypothyroid,goitre,TSH,T3,TT4,Class)
###################### MODELING
#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 10)
set.seed(42)
sample_split <- sample.split(Y = dt_df$Class, SplitRatio = 0.75)
train_set <- subset(x = dt_df, sample_split == TRUE)
test_set <- subset(x = dt_df, sample_split == FALSE)

#fit a decision tree model and use k-fold CV to evaluate performance
dtree_fit_gini <- train(Class~., data = dt_df, method = "rpart", parms = list(split = "gini"), trControl = ctrl)

#Step 5: Evaluate - view summary of k-fold CV               
print(dtree_fit_gini) #metrics give us an idea of how well the model performed on previously unseen data

#view final model
dtree_fit_gini$finalModel
prp(dtree_fit_gini$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#view predictions for each fold
dtree_fit_gini$resample

#Check accuracy
test_pred_gini <- predict(dtree_fit_gini, newdata = test_set)
confusionMatrix(test_pred_gini, test_set$Class )  #check accuracy



###################################3 Pruning
#fit a decision tree model and use k-fold CV to evaluate performance
dtree_fit_gini <- train(Class~., data = dt_df, method = "rpart", parms = list(split = "gini"), trControl = ctrl, tuneLength = 10)

#Step 5: Evaluate - view summary of k-fold CV               
print(dtree_fit_gini) #metrics give us an idea of how well the model performed on previously unseen data

#view final model
dtree_fit_gini$finalModel
prp(dtree_fit_gini$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#view predictions for each fold
dtree_fit_gini$resample

#Check accuracy
test_pred_gini <- predict(dtree_fit_gini, newdata = test_set)
confusionMatrix(test_pred_gini, test_set$Class )  #check accuracy


