install.packages("Metrics")
install.packages("lmvar")
install.packages("devtools")

install.packages("rpart")
install.packages("rpart.plot")
install.packages("devtools")
devtools::install_github("devanshagr/CrossValidation")

library(rpart)
library(rpart.plot)

library(Metrics)
library(lmvar)

#*********************** Data ***********************#
blackbox_train <- read.csv("BlackBoxtrainApril22.csv", stringsAsFactors = FALSE)
blackbox_test <- read.csv("BlackBoxTestApril22-students.csv")
blackbox_submit<- read.csv("BlackBoxTestApril22-submission.csv")

#*********************** Data Summary ***********************#
summary(blackbox_train)

#ID            INPUT1           INPUT2           INPUT3           INPUT4      
#Min.   : 1007   Min.   :  0.00   Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
#1st Qu.:25748   1st Qu.: 25.00   1st Qu.: 24.00   1st Qu.: 25.00   1st Qu.: 24.00  
#Median :50504   Median : 50.00   Median : 50.00   Median : 50.00   Median : 49.00  
#Mean   :50575   Mean   : 49.98   Mean   : 49.74   Mean   : 50.06   Mean   : 49.61  
#3rd Qu.:75403   3rd Qu.: 75.00   3rd Qu.: 75.00   3rd Qu.: 75.00   3rd Qu.: 75.00  
#Max.   :99996   Max.   :100.00   Max.   :100.00   Max.   :100.00   Max.   :100.00  

#SWITCH        SOUND     
#High   :3543   Beep  :1257  
#Low    :3621   Gargle:5727  
#Maximum:3699   Hiss  :1513  
#Medium :3666   Kaboom: 723  
#Minimum:3519   Rumble:2028  
#               Sizzle:3244  
#               Tick  :3556  

#*********************** A Row Count ***********************#
nrow(blackbox_train)
# 18,048 rows


#*********************** Prediction Model ***********************#
treeA <- rpart(SOUND~SWITCH+INPUT1+INPUT2+INPUT3+INPUT4+ID,
               control = rpart.control(minsplit = 90, minbucket = 30, 
                                       cp = 0.000000003, maxdepth = 9),
               data = blackbox_train)
rpart.plot(treeA)

#*********************** Cross Validation ***********************#
CrossValidation::cross_validate(blackbox_train, treeA, 5, 0.8)

# accuracy_subset accuracy_all
#1       0.6750693    0.6540166
#2       0.6761773    0.6548476
#3       0.6836565    0.6587258
#4       0.6905817    0.6703601
#5       0.6811634    0.6631579

#*********************** Writing Model to Test and Submission ***********************#
decision_r <-predict(treeA, newdata = blackbox_test, type = "class")
blackbox_submit$SOUND <-decision_r
write.csv(blackbox_submit, "asilva_blackbox_submit.csv", row.names = FALSE)

